000100 01  W61140.                                                              
000200*                                 URVAL FRÅN W6G3                         
000300*                                                             .           
000400*                                 SELECTED EXTRACT FROM W6G3              
000500*                                                             .           
000600     03 IDDC                 PIC X(2).                                    
000700*                                 IDENTIFIERARE LAGER                     
000800*                                 WAREHOUSE IDENTIFIER                    
000900     03 TIREGDAT             PIC S9(7)           COMP-3.                  
001000*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
001100*                                 REGISTRATION DATE (YYMMDD)              
001200     03 TIKLOCK              PIC S9(9)           COMP-3.                  
001300*                                 KLOCKSLAG (TTMMSSTH)                    
001400*                                 TIME OF DAY (HHMMSSTH)                  
001500     03 IDLOPNRM             PIC S9(9)           COMP-3.                  
001600*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
001700*                                 (0VVDLLLLK)                             
001800*                                 SERIAL NO RECEIVING REPORT              
001900*                                 (0WWDLLLLC)                             
002000     03 IDRADNR              PIC S9(5)           COMP-3.                  
002100*                                 RADNUMMER                               
002200*                                 LINE NO                                 
002300     03 KDINLPRIO            PIC S9(3)           COMP-3.                  
002400*                                 PRIORITETSGRUPP                         
002500*                                 PRIORITY GROUP                          
002600     03 KDINLSTA             PIC X(3).                                    
002700*                                 SYSTEMSTATUS INLEVERANS                 
002800*                                 SYSTEM STATUS RECEIVING                 
002900     03 KDINLUPF             PIC X(4).                                    
003000*                                 UPPFÖLJNINGSSTATUS INLEVERANS           
003100*                                 FOLLOW-UP STATUS RECEIVING              
003200     03 KDINLUPF-NXT         PIC X(4).                                    
003300*                                 NÄSTA UPPFÖLJNINGSSTATUS INLEVE         
003400*                                 RANS                                    
003500*                                 NEXT FOLLOW-UP STATUS RECEIVING         
003600     03 KVINLART             PIC S9(7)           COMP-3.                  
003700*                                 ANTAL I PARTIRAD                        
003800*                                 QTY/LINE IN A LOT                       
003900     03 PRARTSTD             PIC S9(7)V9(2)      COMP-3.                  
004000*                                 ARTIKELSTANDARDPRIS                     
004100*                                 STANDARD PRICE                          
004200     03 FLINLI               PIC X.                                       
004300*                                 INLAGD RAD, PARTI ELLER KOLLI           
004400*                                 STORED  LINE                            
004500     03 KVKOLLI              PIC S9(5)           COMP-3.                  
004600*                                 ANTAL KOLLI                             
004700*                                 NBR OF CASES                            
004800     03 VLARTNTO             PIC S9(8)V9(1)      COMP-3.                  
004900*                                 ARTIKELVOLYM NETTO (CM3)                
005000*                                 PART NET VOLUME    (CM3)                
005100*** END OF VILMAII-COPY LENGTH= 50 BYTES                                  
