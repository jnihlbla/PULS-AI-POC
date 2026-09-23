000100*** EDIT ALLOWED                                                          
000200                                                                          
000300 01  FILLER              PIC X(24)  VALUE 'TEXTER REFILLFÖRSLAG'.         
000400                                                                          
000500 01  TEXTER.                                                              
000600                                                                          
000700     03  REF-TEXT-02        PIC X(12) VALUE '02          '.               
000800     03  REF-TEXT-03        PIC X(12) VALUE '03QBL-REF DC'.               
000900     03  REF-TEXT-05        PIC X(12) VALUE '05NO PRICE  '.               
001000     03  REF-TEXT-06        PIC X(12) VALUE '06L STOCK   '.               
001100     03  REF-TEXT-10        PIC X(12) VALUE '10  REPLD   '.               
001200     03  REF-TEXT-14        PIC X(12) VALUE '1414-MARK   '.               
001300     03  REF-TEXT-15        PIC X(12) VALUE '15 REPING   '.               
003710     03  REF-TEXT-16        PIC X(12) VALUE '16SS BACKED '.               
001400     03  REF-TEXT-18        PIC X(12) VALUE '1805-MARK   '.               
001500     03  REF-TEXT-19        PIC X(12) VALUE '1906-MARK   '.               
001600     03  REF-TEXT-20        PIC X(12) VALUE '2009-MARK   '.               
001700     03  REF-TEXT-21        PIC X(12) VALUE '2125-MARK   '.               
001800     03  REF-TEXT-22        PIC X(12) VALUE '2226-MARK   '.               
001900     03  REF-TEXT-24        PIC X(12) VALUE '2424-MARK   '.               
002000     03  REF-TEXT-25        PIC X(12) VALUE '25REG REPL  '.               
002100     03  REF-TEXT-30        PIC X(12) VALUE '30DEL BLOCK '.               
002200     03  REF-TEXT-33        PIC X(12) VALUE '33H SAL P   '.               
002300     03  REF-TEXT-34        PIC X(12) VALUE '34H SAL W   '.               
002400     03  REF-TEXT-35        PIC X(12) VALUE '35H SAL D   '.               
002500     03  REF-TEXT-40        PIC X(12) VALUE '40    NEW   '.               
002600     03  REF-TEXT-45        PIC X(12) VALUE '45OBSOLETE  '.               
002700     03  REF-TEXT-50        PIC X(12) VALUE '50INVESTIG  '.               
002800     03  REF-TEXT-55        PIC X(12) VALUE '55TRANSF?   '.               
002900     03  REF-TEXT-60        PIC X(12) VALUE '60  TREND   '.               
003000     03  REF-TEXT-62        PIC X(12) VALUE '62 REPL     '.               
003100     03  REF-TEXT-63        PIC X(12) VALUE '63 1002 REPL'.               
003200     03  REF-TEXT-65        PIC X(12) VALUE '65NEXT DEM  '.               
003300     03  REF-TEXT-70        PIC X(12) VALUE '70          '.               
003400     03  REF-TEXT-71        PIC X(12) VALUE '71KIT       '.               
003500     03  REF-TEXT-72        PIC X(12) VALUE '72TPO       '.               
003600     03  REF-TEXT-73        PIC X(12) VALUE '73NEW       '.               
003700     03  REF-TEXT-74        PIC X(12) VALUE '74CAMPAIGN  '.               
003710     03  REF-TEXT-75        PIC X(12) VALUE '75NO REFPART'.               
003800                                                                          
003900 01  FILLER REDEFINES TEXTER.                                             
004000     03  FORSLAG-TEXT OCCURS 33.                                          
004100         05  REF-TEXT-KDREFTEXT      PIC 9(2).                            
004200         05  REF-TEXT                PIC X(10).                           
004300                                                                          
004400 01  REF-TEXT-TABMAX        PIC 9(3)  VALUE 33.                           
004500                                                                          
004600*    VID ÄNDRING, KOMPILERA OM FÖLJANDE PROGRAM                           
004610*    W2035200                                                             
004620*    W2037200                                                             
004630*    W2038200                                                             
004640*    W2039200                                                             
004700*    W2711000                                                             
005300*    W2721000                                                             
