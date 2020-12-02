with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors; 

with Ada.Text_IO; use Ada.Text_IO;

procedure part_one is

    package Expenses_Vectors is new Ada.Containers.Vectors
	(Index_Type => Natural,
	Element_Type => Integer);

    use Expenses_Vectors;


    function Find_Offending_Expense (Expenses: Vector) return Integer;
    function Find_Offending_Expense (Expenses: Vector) return Integer is
    begin
	for I in Expenses.First_Index .. Expenses.Last_Index loop
--	    Put_Line ("I: " & I'Image);
	    declare
		Expense_A : Integer := Expenses (I);
	    begin
		for K in I+1 .. Expenses.Last_Index loop
--		    Put_Line ("K: " & K'Image);
		    declare
			Expense_B : Integer := Expenses (K);
			Total_Expense : Integer := Expense_B + Expense_A;
		    begin
			Put_Line ("Total Expense " & Total_Expense'Image & " For Expense_A " & Expense_A'Image & " and Expense_B " & Expense_B'Image);
			if Total_Expense = 2020 then
			    return Expense_B * Expense_A;
			end if;
		    end;
		end loop;
	    end;
	end loop;
	return -1;
    end Find_Offending_Expense;

    Expenses : Vector;

    Input : File_Type;

    Result : Integer;
begin
    Open (File => Input,
	    Mode => In_File,
	    Name => "input.txt");

    while not End_Of_File (Input) loop
	declare
	    Line : String := Get_Line (Input);
	    Expense : Integer := Integer'Value (Line);
	begin
	    Expenses.Append (Expense);
	end;
    end loop;

    Result := Find_Offending_Expense (Expenses);

    Put_Line ("Result: " & Result'Image);
end;
