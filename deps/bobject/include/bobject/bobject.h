//*****************************************************************************
// Biscuit Object
//
// File:   bobject.h
// Author: Martin Dorazil
// Date:   27/10/2017
//
// Copyright 2017 Martin Dorazil
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
//*****************************************************************************

#ifndef BOBJECT_H_5TXSHNOP
#define BOBJECT_H_5TXSHNOP

#include <stddef.h>
#include <stdbool.h>
#include "bobject/batomic.h"
#include "bobject/config.h"
#include "bobject/types.h"
#include "bobject/utils.h"
#include "bobject/bmemory.h"

BO_BEGIN_DECLS

/**
 * Result of object copy constructor call.
 */
typedef enum bo_copy_result {
  /* Copying of object is disabled */
  BO_NO_COPY,

  /* Use default memcpy on data */
  BO_COPY_DEFAULT,

  /* Copy was created sucessfully */
  BO_COPY_SUCCESS,

  /* Copying failed */
  BO_COPY_FAIL
} bo_copy_result;

#define _bo_klass_body(name, base) \
  struct _##base##Klass *parent; \
  struct _##name##Vtable *vtable; \
  const size_t size; \
  const size_t vtab_size; \
  const char *type_name; \
  \
  bool initialized; \
  \
  void \
  (* const initializer)(void *); \
  \
  void \
  (* const ctor)(void *, void *p); \
  \
  void \
  (* const dtor)(void *); \
  \
  bo_copy_result \
  (* const copy)(void *, void *)


/**
 * Every class type is based on this type.
 */
typedef struct _Klass *BType;
struct _Klass
{
  _bo_klass_body(,);
};

/**
 * Prepare new class type with name and base class.
 * After this macro comes virtual function definitions.
 *
 * Produced types:
 *  name
 *  nameObj
 *  nameKlass
 *  nameVtable
 *
 * @param name Object name.
 * @param base Base type name.
 * @note Must end with bo_end().
 */
#define bo_decl_type_begin(name, base) \
  typedef struct _##name (name); \
  typedef struct _##name##Params name##Params; \
  extern BO_EXPORT struct _##name##Klass g_##name##Klass; \
  extern BO_EXPORT struct _##name##Vtable g_##name##Vtable; \
  struct _##name##Klass { \
    _bo_klass_body(name, base);\
  }; \
  \
  struct _##name##Vtable { \
    struct _##base##Vtable base##Vtable;

/**
 * Declare nameParam structure as parameter pack passed
 * into object constructor. Write list of parameters after
 * this macro separated by coma.
 *
 * @param name Object name.
 * @note Must end with bo_end().
 */
#define bo_decl_params_begin(name) \
  struct _##name##Params { \
    char __void__;

/**
 * Declare nameParam structure as parameter pack passed
 * into object constructor. Write list of parameters after
 * this macro separated by coma.
 *
 * Base type will be added at the begining of generated structure.
 *
 * @param name Object name.
 * @param base Base type name.
 * @note Must end with bo_end().
 */
#define bo_decl_params_with_base_begin(name, _base) \
  struct _##name##Params { \
    struct _##_base##Params base;

/**
 * Declare nameObj structure with proper base class containing
 * all objects data members. Write list of members after
 * this macro separated by coma.
 * When members should be private, put this declaration
 * into separate header impl_name.h
 *
 * @param name Object name.
 * @param base Base type name.
 * @note Must end with bo_end().
 */
#define bo_decl_members_begin(name, _base) \
  struct _##name { \
    struct _##_base base;

/**
 * Implement class type. This macro produce forward declarations
 * of nameKlass_init, name_ctor, name_dtor and name_copy methods.
 * These must be implemented for every class. Global class table
 * is set here.
 * @param name Object name.
 * @param base Base type name.
 */
#define bo_impl_type(name, base) \
  typedef struct _##name##Klass name##Klass;\
  static void \
  name##Klass_init(name##Klass *klass); \
  \
  static void \
  name##_ctor(name *self, name##Params *p); \
  \
  static void \
  name##_dtor(name *self); \
  \
  static bo_copy_result \
  name##_copy(name *self, name *other); \
  \
  struct _##name##Vtable g_##name##Vtable = {0};\
  \
  struct _##name##Klass g_##name##Klass = \
    { \
      .parent = &g_##base##Klass, \
      .vtable = &g_##name##Vtable, \
      .size = sizeof(struct _##name), \
      .vtab_size = sizeof(struct _##name##Vtable), \
      .ctor = (void (* const)(void *, void *)) name##_ctor, \
      .dtor = (void (* const)(void *)) name##_dtor, \
      .copy = (bo_copy_result (* const)(void *, void *)) name##_copy, \
      .initializer = (void (* const)(void *)) name##Klass_init, \
      .initialized = false, \
      .type_name = #name\
    } \

/**
 * End of declaration blocks.
 */
#define bo_end() }

/* helpers */
/**
 * Get size of type in bytes.
 */
#define bo_sizeof(name) \
  (g_##name##Klass.size)

/**
 * Get name of type.
 */
#define bo_typeof(name) \
  ((BType) &g_##name##Klass)

/**
 * Create new instance of type on heap and set reference
 * count to one.
 * Constructor is called here.
 *
 * @param name Type name.
 * @param params Pointer to constructor parameters pack.
 * @return New created object.
 */
#define bo_new(name, params) \
  (name *) _bo_new((struct _Klass *)& g_##name##Klass, params)

/**
 * Atomically increase reference count of the object by one and
 * return pointer to the object.
 *
 * @param obj Object.
 * @return Same object.
 */
#define bo_ref(obj) \
  (void *)_bo_ref((BObject *) (obj))

/**
 * Atomically decrease reference count of the object. When
 * reference count reaches 0 object is automatically deleted.
 * @param obj Object.
 */
#define bo_unref(obj) \
  _bo_unref((BObject *) (obj))

/**
 * Get reference count of the object.
 * @param obj Object.
 * @return Reference count.
 */
#define bo_ref_count(obj) \
  _bo_ref_count((BObject *) (obj))

/**
 * Create copy of the object with reference count set to one.
 * @param obj Object to be duplicated.
 * @return Object copy.
 * @note Object must be copyable.
 */
#define bo_duplicate(obj) \
  (void *)_bo_duplicate((BObject *) (obj))

/**
 * Call to parent constructor inside child constructor.
 * @param parent Parent type name.
 * @param params Parameters pack passed into parent constructor.
 */
#define bo_parent_ctor(parent, params) \
  if (g_##parent##Klass.ctor != NULL)\
    g_##parent##Klass.ctor(&self->base, params)

/**
 * Get virtual table from object instance.
 * @param obj Pointer to object instance.
 * @param name Type name.
 */
#define bo_vtbl(obj, name) \
  ((struct _##name##Vtable *)((BObject *)&(obj)->base)->type->vtable)

/**
 * Get virtual table from class table.
 * @param obj Pointer to class table.
 * @param name Type name.
 */
#define bo_vtbl_cl(cl, name) \
  ((struct _##name##Vtable *)(cl)->vtable)

/**
 * Get object members for instance of type.
 */
#define bo_members(obj, name) \
  ((name *) (obj))

/**
 * @return true when obj is type of type
 */
#define bo_is_typeof(obj, type) \
  ((*(struct _##type##Klass **)(obj)) == &g_##type##Klass)

/**
 * @return object name as const char *
 */
#define bo_type_name(obj) \
  ((BObject *)(obj))->type->type_name


/* init base object type */
typedef struct _BObject BObject;
struct _BObject
{
  struct _Klass *type;
  bo_atomic_int ref_count;
};

struct _BObjectKlass
{
  _bo_klass_body(BObject,);
};

struct _BObjectVtable
{
  char __void__;
};

extern BO_EXPORT struct _BObjectKlass g_BObjectKlass;
extern BO_EXPORT struct _BObjectVtable g_BObjectVtable;
/* init base object type end */

/* public functions */
extern BO_EXPORT void *
_bo_new(struct _Klass *cl, void *p);

extern BO_EXPORT BObject *
_bo_ref(BObject *obj);

extern BO_EXPORT void
_bo_unref(BObject *obj);

extern BO_EXPORT int
_bo_ref_count(BObject *obj);

extern BO_EXPORT BObject *
_bo_duplicate(BObject *src);

BO_END_DECLS

#endif /* end of include guard: BOBJECT_H_5TXSHNOP */
