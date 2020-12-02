with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors; 

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Calendar; use Ada.Calendar;

procedure part_two is

    package Expenses_Vectors is new Ada.Containers.Vectors
	(Index_Type => Natural,
	Element_Type => Integer);

    use Expenses_Vectors;


    function Find_Offending_Expense (Expenses: Vector) return Integer;
    function Find_Offending_Expense (Expenses: Vector) return Integer is
    begin
	for I in Expenses.First_Index .. Expenses.Last_Index loop
	    for K in I+1 .. Expenses.Last_Index loop
		for C in K+1 .. Expenses.Last_Index loop
		    declare
			Expense_A : Integer := Expenses (I);
			Expense_B : Integer := Expenses (K);
			Expense_C : Integer := Expenses (C);
			Total_Expense : Integer := Expense_B + Expense_A + Expense_C;
		    begin
			if Total_Expense = 2020 then
			    return Expense_B * Expense_A * Expense_C;
			end if;
		    end;
		end loop;
	    end loop;
	end loop;
	return -1;
    end Find_Offending_Expense;

    Expenses : Vector;

    Input : File_Type;

    Result : Integer;

    Start_Time : Time := Clock;
    Finish_Time : Time;
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

    Finish_Time := Clock;

    Put_Line ("Total Time Taken: " & Duration(Start_Time - Finish_Time)'Image);
end;
