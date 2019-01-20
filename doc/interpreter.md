
# Table of Contents

1.  [Documentation for BL-MIR](#orgfb0b7e5)
    1.  [Instructions:](#org97c6e90)
        1.  [const](#orgcc67c03)
        2.  [decl](#orge879101)
        3.  [load](#orga356d89)
        4.  [store](#orgf26cc34)
        5.  [call](#org43b272c)


<a id="orgfb0b7e5"></a>

# Documentation for BL-MIR

BL-MIR (Biscuit Language Middle Intermediate Representation) is a simplified representation of the Biscuit Language created from AST. It is located between AST and LLVM-IR.


<a id="org97c6e90"></a>

## Instructions:


<a id="orgcc67c03"></a>

### const

Constant value.

    <T> const <value> // yields constant value of the type T

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">stack op</th>
<th scope="col" class="org-left">data</th>
<th scope="col" class="org-left">description</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">PUSH</td>
<td class="org-left">value</td>
<td class="org-left">constant value</td>
</tr>
</tbody>
</table>


<a id="orge879101"></a>

### decl

Variable declaration.

    <*T> decl <name> : <T> // yields *T (pointer to value allocated on the stack)

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">stack op</th>
<th scope="col" class="org-left">data</th>
<th scope="col" class="org-left">description</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">PUSH</td>
<td class="org-left">-</td>
<td class="org-left">storage for the variable</td>
</tr>
</tbody>
</table>


<a id="orga356d89"></a>

### load

Push pointed value on the stack.

    <T> load <ptr> // yields T loaded from ptr

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">stack op</th>
<th scope="col" class="org-left">data</th>
<th scope="col" class="org-left">description</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">POP</td>
<td class="org-left">ptr</td>
<td class="org-left">pointer to source</td>
</tr>


<tr>
<td class="org-left">PUSH</td>
<td class="org-left">value</td>
<td class="org-left">value loaded from source</td>
</tr>
</tbody>
</table>


<a id="orgf26cc34"></a>

### store

Store value from source to destination address.

    void store <src> -> <dest_ptr> // yields void

Destination pointer is on the stack:

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">stack op</th>
<th scope="col" class="org-left">data</th>
<th scope="col" class="org-left">description</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">POP</td>
<td class="org-left">dest<sub>ptr</sub></td>
<td class="org-left">pointer to destination</td>
</tr>


<tr>
<td class="org-left">POP</td>
<td class="org-left">src<sub>ptr</sub></td>
<td class="org-left">value</td>
</tr>
</tbody>
</table>

Destination pointer is declaration:

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">stack op</th>
<th scope="col" class="org-left">data</th>
<th scope="col" class="org-left">description</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">POP</td>
<td class="org-left">src<sub>ptr</sub></td>
<td class="org-left">value</td>
</tr>
</tbody>
</table>


<a id="org43b272c"></a>

### call

pc   program counter (pointer to current instruction)
RA   return address (used later for rollback of the stack)

call fn (1, 2, 3) 4

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">stack op</th>
<th scope="col" class="org-left">data</th>
<th scope="col" class="org-left">instr</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">PUSH</td>
<td class="org-left">3</td>
<td class="org-left">?</td>
</tr>


<tr>
<td class="org-left">PUSH</td>
<td class="org-left">2</td>
<td class="org-left">?</td>
</tr>


<tr>
<td class="org-left">PUSH</td>
<td class="org-left">1</td>
<td class="org-left">?</td>
</tr>


<tr>
<td class="org-left">PUSH RA</td>
<td class="org-left">pc, prev RA</td>
<td class="org-left">call</td>
</tr>


<tr>
<td class="org-left">&#x2026;</td>
<td class="org-left">-</td>
<td class="org-left">-</td>
</tr>


<tr>
<td class="org-left">POP RA</td>
<td class="org-left">-</td>
<td class="org-left">ret</td>
</tr>


<tr>
<td class="org-left">POP</td>
<td class="org-left">-</td>
<td class="org-left">ret</td>
</tr>


<tr>
<td class="org-left">POP</td>
<td class="org-left">-</td>
<td class="org-left">ret</td>
</tr>


<tr>
<td class="org-left">POP</td>
<td class="org-left">-</td>
<td class="org-left">ret</td>
</tr>


<tr>
<td class="org-left">PUSH</td>
<td class="org-left">4</td>
<td class="org-left">ret</td>
</tr>
</tbody>
</table>

