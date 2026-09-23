000100*COMPOPT STDSUB=YES                                                       
000200                                                                          
000300 ID DIVISION.                                                             
000400 PROGRAM-ID.                 W009VADD.                                    
000500*AUTHOR.                     IDK, GÖTEBORG.                               
000600*DATE-WRITTEN.               NOV 1978.                                    
000700*    SKIP3                                                                
000800*        PROGRAMMET ADDERAR GODTYCKLIGT ANTAL VECKOR (ANTAL) TILL         
000900*        EN DATUMANGIVELSE I FORM AV ÅÅVV. RESULTATET ANGES               
001000*        I DATUMANGIVELSEN.                                               
001100*        ANTALET KAN VARA NEGATIVT.                                       
001200*        DET FÖRUTSÄTTS 52 VECKOR PER ÅR.                                 
001300*        -- NOT CHECKED BY WY2000. MANUELLT KONVERTERAD                   
001400                                                                          
001500 ENVIRONMENT DIVISION.                                                    
001600     SKIP3                                                                
001700 DATA DIVISION.                                                           
001800     EJECT                                                                
001900 WORKING-STORAGE SECTION.                                                 
002000                                                                          
002100     SKIP3                                                                
002200 77  IDPGM                   PIC X(8)    VALUE 'W009VADD'.                
002300                                                                          
002400 01  ABEND                   PIC X(8)    VALUE 'ABEND   '.                
002500 01  RKOD                    PIC S9(4)   VALUE +0    COMP SYNC.           
002600                                                                          
002700 01  ARBETSAREOR.                                                         
002800     03  W-DATUM-X.                                                       
002900       05 W-DATUM            PIC 9(4).                                    
003000 01  SUBPROGRAM.                                                          
003100     03  WZ20DAYS            PIC X(8)    VALUE 'WZ20DAYS'.                
003200     EJECT                                                                
003300*    --- PARAMETRAR TILL SUBPROGRAM WZ20DAYS                              
003400 01  FILLER                      PIC X(16)   VALUE 'WZ20DAYS'.            
003500*01 -COPY WZ20DAYS                                                        
003600     EJECT                                                                
003700 LINKAGE SECTION.                                                         
003800                                                                          
003900 01  DATUM-AAVV              PIC S9(5)               COMP-3.              
004000 01  ANTAL                   PIC S9(3)               COMP-3.              
004100     EJECT                                                                
004200 PROCEDURE DIVISION USING DATUM-AAVV ANTAL.                               
004300                                                                          
004400     MOVE DATUM-AAVV             TO W-DATUM                               
004500     IF W-DATUM-X(3:2) = '00'                                             
004600       ADD 1 TO W-DATUM                                                   
004700     END-IF                                                               
004800     MOVE W-DATUM                TO DAYS-TIDATE1                          
004900     MOVE 'YYWW'                 TO DAYS-KDDATFMT1                        
005000                                    DAYS-KDDATFMT2                        
005100     MOVE SPACE                  TO DAYS-IDCALEND                         
005200                                    DAYS-TIDATE2                          
005300     MULTIPLY ANTAL BY 7     GIVING DAYS-KVDAYS                           
005400                                                                          
005500     CALL WZ20DAYS            USING DAYS-WZ20DAYS                         
005600                                                                          
005700     MOVE DAYS-TIDATE2(1:4)      TO W-DATUM                               
005800     MOVE W-DATUM                TO DATUM-AAVV                            
005900                                                                          
006000     MOVE ZERO TO RETURN-CODE                                             
006100     GOBACK                                                               
006200     .                                                                    
