000100*** EDIT ALLOWED                                                          
000200*                            *************************************        
000300*                            *** ANVÄNDS FÖR ATT RÄKNA OCH                
000400*                            *** KONTROLLERA ANTALET POSTER AV            
000500*                            *** OLIKA TYPER I FILEN.                     
000600*                            *** DIRECT BUSINESS.                         
000700*                            *************************************        
000800*                                                                         
000900*   TABELL FÖR ATT SÖKA LEVERANTöRSNUMMER.                                
001000*                                                                         
001010 01  POSTTYP-MAX-IX PIC S9(3) VALUE 24.                                   
001020*                                                                         
001100 01  POSTTYPER.                                                           
001200************************************TIXCOUNT                              
001300     03  FILLER    PIC X(8)  VALUE 'B0100000'.                            
001400     03  FILLER    PIC X(8)  VALUE 'C0200000'.                            
001500     03  FILLER    PIC X(8)  VALUE 'D0300000'.                            
001600     03  FILLER    PIC X(8)  VALUE 'E0400000'.                            
001700     03  FILLER    PIC X(8)  VALUE 'F0500000'.                            
001800     03  FILLER    PIC X(8)  VALUE 'G0600000'.                            
001900     03  FILLER    PIC X(8)  VALUE 'H0700000'.                            
002000     03  FILLER    PIC X(8)  VALUE 'I0800000'.                            
002100     03  FILLER    PIC X(8)  VALUE 'J0900000'.                            
002200     03  FILLER    PIC X(8)  VALUE 'K1000000'.                            
002300     03  FILLER    PIC X(8)  VALUE 'L1100000'.                            
002400     03  FILLER    PIC X(8)  VALUE 'M1200000'.                            
002500     03  FILLER    PIC X(8)  VALUE 'N1300000'.                            
002600     03  FILLER    PIC X(8)  VALUE 'O1400000'.                            
002700     03  FILLER    PIC X(8)  VALUE 'P1500000'.                            
002800     03  FILLER    PIC X(8)  VALUE 'Q1600000'.                            
002900     03  FILLER    PIC X(8)  VALUE 'R1700000'.                            
002910     03  FILLER    PIC X(8)  VALUE 'S1800000'.                            
002920     03  FILLER    PIC X(8)  VALUE 'T1900000'.                            
002930     03  FILLER    PIC X(8)  VALUE 'U2000000'.                            
002940     03  FILLER    PIC X(8)  VALUE 'V2100000'.                            
002950     03  FILLER    PIC X(8)  VALUE 'W2200000'.                            
002960     03  FILLER    PIC X(8)  VALUE 'X2300000'.                            
002970     03  FILLER    PIC X(8)  VALUE 'Y2400000'.                            
003000*                                                                         
003100 01  POSTTYP-TAB          REDEFINES POSTTYPER.                            
003200     03  POSTTYP-INGANG   OCCURS 24 TIMES                                 
003300                          ASCENDING KEY IS POSTTYP-SOK                    
003400                          INDEXED BY POSTTYP-IX.                          
003500       05  POSTTYP-SOK         PIC X(1).                                  
003600       05  POSTTYP-INDEX       PIC 9(2).                                  
003700       05  POSTTYP-RAKNARE     PIC 9(5).                                  
004200*                                                                         
004300*                                                                         
004400*** END COPY W463RSUM    LENGTH=192                                       
