000010*** EDIT ALLOWED                                                          
000010*                                        VID ÄNDRING:                     
000020*                                        KOM IHÅG OCCURS OCH MAX          
000100 01  TAB-IDINK.                                                           
000200*                                 AUKTORISATION AV INKÖPARE               
000300*                                 VID NYKÖP TILL VTC (GPO)                
001000     03 FILLER               PIC 9(3)    VALUE  34.                       
001100     03 FILLER               PIC 9(3)    VALUE  59.                       
001500*                                        BELGIEN                          
001600     03 FILLER               PIC 9(3)    VALUE 300.                       
001700     03 FILLER               PIC 9(3)    VALUE 499.                       
001710*                                        GÖTEBORG                         
001720     03 FILLER               PIC 9(3)    VALUE 500.                       
001730     03 FILLER               PIC 9(3)    VALUE 534.                       
002100*                                        SKÖVDE/KÖPING                    
002200     03 FILLER               PIC 9(3)    VALUE 670.                       
002300     03 FILLER               PIC 9(3)    VALUE 699.                       
002400*                                        PARTS  (EJ TILL LV !)            
002500     03 FILLER               PIC 9(3)    VALUE 730.                       
002600     03 FILLER               PIC 9(3)    VALUE 749.                       
002700*                                        USA                              
002800     03 FILLER               PIC 9(3)    VALUE 835.                       
002900     03 FILLER               PIC 9(3)    VALUE 844.                       
003000*                                        LEYLAND                          
003100     03 FILLER               PIC 9(3)    VALUE 890.                       
003200     03 FILLER               PIC 9(3)    VALUE 895.                       
003900*                                        SCOTLAND                         
004000 01  TABELL-IDINK            REDEFINES TAB-IDINK.                         
004100     03  FILLER              OCCURS  7.                                   
004200         05  TAB-IDINK-FOM   PIC 9(3).                                    
004300         05  TAB-IDINK-TOM   PIC 9(3).                                    
004400*                                                                         
004500 01  IX-TAB-IDINK            PIC S9(3) COMP-3.                            
004600 01  IX-TAB-IDINK-MAX        PIC S9(3) COMP-3  VALUE +7.                  
