000100 01  W61128.                                                              
000200*                                 URVAL FRÅN W6FILA MED DAGENS            
000300*                                 HÄDELSER KOMPLETTERAD MED ARTNR         
000400*                                 OCH VOLYM FRÅN W6INLA                   
000500*                                                               .         
000600*                                 TODAYS W6FILA ACTIONS COMPLE-           
000700*                                 MENTED WITH PARTNO AND VOLUME           
000800*                                 FROM W6INLA                             
000900*                                                               .         
001000     03 IDDC                 PIC X(2).                                    
001100*                                 IDENTIFIERARE LAGER                     
001200*                                 WAREHOUSE IDENTIFIER                    
001300     03 TIREGDAT             PIC S9(7)           COMP-3.                  
001400*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
001500*                                 REGISTRATION DATE (YYMMDD)              
001600     03 TIKLOCK              PIC S9(9)           COMP-3.                  
001700*                                 KLOCKSLAG (TTMMSSTH)                    
001800*                                 TIME OF DAY (HHMMSSTH)                  
001900     03 IDARTNR              PIC S9(9)           COMP-3.                  
002000*                                 ARTIKELNUMMER                           
002100*                                 PART NUMBER                             
002200     03 IDLOPNRM             PIC S9(9)           COMP-3.                  
002300*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
002400*                                 (0VVDLLLLK)                             
002500*                                 SERIAL NO RECEIVING REPORT              
002600*                                 (0WWDLLLLC)                             
002700     03 IDRADNR              PIC S9(5)           COMP-3.                  
002800*                                 RADNUMMER                               
002900*                                 LINE NO                                 
003000     03 KDINLPRIO            PIC S9(3)           COMP-3.                  
003100*                                 PRIORITETSGRUPP                         
003200*                                 PRIORITY GROUP                          
003300     03 KDINLSTA             PIC X(3).                                    
003400*                                 SYSTEMSTATUS INLEVERANS                 
003500*                                 SYSTEM STATUS RECEIVING                 
003600     03 KDINLUPF             PIC X(4).                                    
003700*                                 UPPFÖLJNINGSSTATUS INLEVERANS           
003800*                                 FOLLOW-UP STATUS RECEIVING              
003900     03 KVINLART             PIC S9(7)           COMP-3.                  
004000*                                 ANTAL I PARTIRAD                        
004100*                                 QTY/LINE IN A LOT                       
004200     03 PRARTSTD             PIC S9(7)V9(2)      COMP-3.                  
004300*                                 ARTIKELSTANDARDPRIS                     
004400*                                 STANDARD PRICE                          
004500     03 KVKOLLI              PIC S9(5)           COMP-3.                  
004600*                                 ANTAL KOLLI                             
004700*                                 NBR OF CASES                            
004800     03 FLINLI               PIC X.                                       
004900*                                 INLAGD RAD, PARTI ELLER KOLLI           
005000*                                 STORED  LINE                            
005100*** END OF VILMAII-COPY LENGTH= 46 BYTES                                  
