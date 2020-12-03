with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Maps; use Ada.Strings.Maps;

with Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Vectors;

procedure Part_One is
    Input_File_Name : Unbounded_String := To_Unbounded_String("input.txt");
    Input_File : File_Type;

    type Password is record
	Password : Unbounded_String;
	Policy_Rule : Character;
	Policy_Max : Natural;
	Policy_Min : Natural;
    end record;

    package Password_Vectors is new Ada.Containers.Vectors
	(Index_Type => Natural,
	Element_Type => Password);

    Passwords : Password_Vectors.Vector;

    package String_Vectors is new Ada.Containers.Indefinite_Vectors (Natural, String);

    function Tokenize (Input : String; Delimiter : Character := ' ') return String_Vectors.Vector is
	Start : Positive := Input'First;
	Finish : Natural := 0;
	Output : String_Vectors.Vector;
    begin
	Put_Line ("Delimiter -" & Delimiter & "-");
	while Start <= Input'Last loop
	    Find_Token (Input, To_Set (Delimiter), Start, Outside, Start, Finish);
	    exit when Start > Finish;
	    Output.Append (Input (Start .. Finish));
	    Start := Finish + 1;
	end loop;
	return Output;
    end Tokenize;

    Valid_Passwords : Natural := 0;
begin

    if Argument_Count >= 1 then
	Input_File_Name := To_Unbounded_String (Argument (1));

	Put_Line (To_String(Input_File_Name));
    end if;

    Open (File => Input_File, Mode => In_File, Name => To_String(Input_File_Name));
    loop
	exit when End_Of_File (Input_File);
	
	declare
	    Line : String := Get_Line (Input_File);

	    P : Password;


	begin
	    declare 
		Parts : String_Vectors.Vector := Tokenize (Line);
		-- Policy_Range : String_Vectors.Vector := Tokenize (Parts(0), '-');
		Raw_Policy_Rule : String := Parts(1);
		Raw_Policy : String_Vectors.Vector := Tokenize (Parts(0), '-');
		Policy_Min : Natural := Natural'Value(Raw_Policy(0));
		Policy_Max : Natural := Natural'Value(Raw_Policy(1));
		Policy_Rule : Character := Raw_Policy_Rule(Raw_Policy_Rule'First);
		Count : Natural; 
	    begin
		P := Password'(
		    Password => To_Unbounded_String (Parts(2)),
		    Policy_Max => Policy_Max,
		    Policy_Min => Policy_Min,
		    Policy_Rule => Policy_Rule
		    );

		Count := Ada.Strings.Unbounded.Count (P.Password, To_Set(P.Policy_Rule));
		Put_Line ("Count: " & Count'Image);
		Put_Line ("Password: " & To_String (P.Password));
		Put_Line ("Policy: " & P.Policy_Rule);
		Put_Line ("Policy_Max " & P.Policy_Max'Image);
		Put_Line ("Policy_Min " & P.Policy_Min'Image);
		if Count <= P.Policy_Max and Count >= P.Policy_Min then
		    Put_Line ("Valid");
		    Valid_Passwords := Valid_Passwords + 1;
		else 
		    Put_Line ("Invalid");
		end if;
	    end;
	end;
    end loop;
    Put_Line ("Valid Passwords: " & Valid_Passwords'Image);
end Part_One;
