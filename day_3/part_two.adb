with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;

procedure Part_Two is
    type Spot is (Land, Tree, Unknown);
    Input_File_Name : String := (if Argument_Count >= 1 
				    then Argument(1) 
				    else "input.txt");

    package Row_Vectors is new Ada.Containers.Vectors (Index_Type => Natural, Element_Type => Spot);
    use Row_Vectors;
    package Map_Vectors is new Ada.Containers.Vectors (Index_Type => Natural, Element_Type => Row_Vectors.Vector);

    Map : Map_Vectors.Vector;

    function To_String (Item: Row_Vectors.Vector) return String is
	use Ada.Strings.Unbounded;
	Result : Unbounded_String;
    begin
	for E of Item loop
	    Result := Result & " " & E'Image;
	end loop;
		
	return To_String (Result);
    end;

    function "+" (Right : Row_Vectors.Vector) return String is
    begin
	return To_String (Right);
    end "+";

    function "+" (Left : in String; Right : in Row_Vectors.Vector) return String is
    begin
	return Left & To_String (Right);
    end "+";

    Input_File : File_Type;
begin
    
    Put_Line (Input_File_Name);

    Open (Input_File, In_File, Input_File_Name);
    loop
	exit when End_Of_File (Input_File); 
	declare
	    Line : String := Get_Line (Input_File);
	    Row : Row_Vectors.Vector;
	begin
	    for Character of Line loop
		case Character is
		    when '#' => Row.Append (Tree);
		    when '.' => Row.Append (Land);
		    when others => Row.Append (Unknown);
		end case;
	    end loop;
	    Map.Append (Row);
	end;
    end loop;

    declare
	X : Natural := 0;
	Y : Natural := 0;
	X_Increment : constant Natural := 1;
	Y_Increment : constant Natural := 2;
	Trees_Hit : Natural := 0;
    begin
	while Y < Natural (Map.Length) loop
	    declare
		Row : Row_Vectors.Vector := Map (Y);
		Length : Natural := Natural (Row.Length);
	    begin
		if X >= Length then
		    X := X - Length;
		end if;
		if Row (X) = Tree then
		    Trees_Hit := Trees_Hit + 1;
		end if;

		X := X + X_Increment;
		Y := Y + Y_Increment;
	    end;
	end loop;
	Put_Line ("Trees Hit: " & Trees_Hit'Image);
    end;
end Part_Two;
