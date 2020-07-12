//************************************************************************************************
// bl
//
// File:   puml_export.cpp
// Author: Martin Dorazil
// Date:   7/12/20
//
// Copyright 2020 Martin Dorazil
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.
//************************************************************************************************

#include "common.h"

#include <algorithm>
#include <fstream>
#include <unordered_map>
#include <vector>
_SHUT_UP_BEGIN
#include "builder.h"
_SHUT_UP_END

struct PumlUnit {
	std::string              name;
	std::vector<std::string> entries;
};

struct PumlAssembly {
	const char *                               name;
	std::unordered_map<const Unit *, PumlUnit> units;
	std::vector<std::string>                   deps;
};

std::unordered_map<const Assembly *, PumlAssembly> assemblies;

static std::string get_unit_name(const Unit *unit)
{
	auto name = std::string(unit->filename);
	return name.substr(0, name.find_last_of('.'));
}

static PumlAssembly &fetch_assembly(const Assembly *assembly)
{
	const auto it = assemblies.find(assembly);
	if (it != assemblies.end()) return it->second;
	PumlAssembly tmp;
	tmp.name = assembly->name;
	return assemblies.insert({assembly, tmp}).first->second;
}

static PumlUnit &fetch_unit(PumlAssembly &pa, const Unit *unit)
{
	const auto it = pa.units.find(unit);
	if (it != pa.units.end()) return it->second;
	PumlUnit tmp;
	tmp.name = get_unit_name(unit);
	if (unit->loaded_from) {
		auto *const loaded_from = unit->loaded_from->location.unit;
		if (loaded_from) pa.deps.push_back(tmp.name + " <- " + get_unit_name(loaded_from));
	}
	return pa.units.insert({unit, tmp}).first->second;
}

static void add_fn(PumlUnit &pu, const std::string &name)
{
	pu.entries.emplace_back(name);
}

static void export_assembly(PumlAssembly &pa)
{
	std::ofstream file;
	file.open(std::string(pa.name) + ".puml");
	file << "@startuml\n";
	for (auto &it : pa.units) {
		auto &pu = it.second;
		std::sort(pu.entries.begin(),
		          pu.entries.end(),
		          [](const std::string &a, const std::string &b) { return a < b; });

		file << "class " << pu.name << " {\n";
		for (const auto &entry : pu.entries) {
			file << entry << "\n";
		}
		file << "}\n";
	}

	for (auto &dep : pa.deps) {
		file << dep << "\n";
	}
	file << "@enduml\n";
	file.close();
}

// public
extern "C" void puml_message_handler(const Assembly *assembly, const BuilderMessage *msg)
{
	switch (msg->kind) {
	case BUILDER_MSG_ASSEMBLY_END: {
		auto &pa = fetch_assembly(assembly);
		export_assembly(pa);
		break;
	}
	case BUILDER_MSG_FN: {
		MirFn *fn = msg->data.fn;
		if (!fn) break;
		if (!fn->is_global) break;
		if (!fn->id) break;
		if (!fn->decl_node) break;
		const bool is_private = fn->decl_node->owner_scope->kind == SCOPE_PRIVATE;
		auto &     pa         = fetch_assembly(assembly);
		auto &     pu         = fetch_unit(pa, fn->decl_node->location->unit);
		add_fn(pu,
		       is_private ? ("-" + std::string(fn->id->str) + "()")
		                  : ("+" + std::string(fn->id->str) + "()"));
		break;
	}
	default:
		break;
	}
}
