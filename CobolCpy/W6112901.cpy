000100 01  W61128.                                                              
000200*                                 URVAL FRÅN W6FILA                       
000300*                                 MED DAGENS HÄNDELSER                    
000400*                                 KOMPLETTERAD MED ARTNR,                 
000500*                                 VOLYM FRÅN W6INLA OCH GLT               
000600*                                                               .         
000700*                                 TODAYS W6FILA ACTIONS COMPLE-           
000800*                                 MENTED WITH PARTNO AND VOLUME           
000900*                                 FROM W6INLA AND WORK FLOW TIME          
001000*                                                               .         
001100     03 IDDC                 PIC X(2).                                    
001200*                                 IDENTIFIERARE LAGER                     
001300*                                 WAREHOUSE IDENTIFIER                    
001400     03 TIREGDAT             PIC S9(7)           COMP-3.                  
001500*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
001600*                                 REGISTRATION DATE (YYMMDD)              
001700     03 TIKLOCK              PIC S9(9)           COMP-3.                  
001800*                                 KLOCKSLAG (TTMMSSTH)                    
001900*                                 TIME OF DAY (HHMMSSTH)                  
002000     03 IDARTNR              PIC S9(9)           COMP-3.                  
002100*                                 ARTIKELNUMMER                           
002200*                                 PART NUMBER                             
002300     03 IDLOPNRM             PIC S9(9)           COMP-3.                  
002400*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
002500*                                 (0VVDLLLLK)                             
002600*                                 SERIAL NO RECEIVING REPORT              
002700*                                 (0WWDLLLLC)                             
002800     03 IDRADNR              PIC S9(5)           COMP-3.                  
002900*                                 RADNUMMER                               
003000*                                 LINE NO                                 
003100     03 KDINLPRIO            PIC S9(3)           COMP-3.                  
003200*                                 PRIORITETSGRUPP                         
003300*                                 PRIORITY GROUP                          
003400     03 KDINLSTA             PIC X(3).                                    
003500*                                 SYSTEMSTATUS INLEVERANS                 
003600*                                 SYSTEM STATUS RECEIVING                 
003700     03 KDINLUPF             PIC X(4).                                    
003800*                                 UPPFÖLJNINGSSTATUS INLEVERANS           
003900*                                 FOLLOW-UP STATUS RECEIVING              
004000     03 KDINLUPF-NXT         PIC X(4).                                    
004100*                                 NÄSTA UPPFÖLJNINGSSTATUS INLEVE         
004200*                                 RANS                                    
004300*                                 NEXT FOLLOW-UP STATUS RECEIVING         
004400     03 KVINLART             PIC S9(7)           COMP-3.                  
004500*                                 ANTAL I PARTIRAD                        
004600*                                 QTY/LINE IN A LOT                       
004700     03 PRARTSTD             PIC S9(7)V9(2)      COMP-3.                  
004800*                                 ARTIKELSTANDARDPRIS                     
004900*                                 STANDARD PRICE                          
005000     03 TIGLT                PIC S9(3)V9(2)      COMP-3.                  
005100*                                 UTFÖRD ARBETSTID (TIMMAR)               
005200*                                 PERFORMED LABOUR TIME (HOURS)           
005300     03 VLARTNTO             PIC S9(8)V9(1)      COMP-3.                  
005400*                                 ARTIKELVOLYM NETTO (CM3)                
005500*                                 PART NET VOLUME    (CM3)                
005600*** END OF VILMAII-COPY LENGTH= 54 BYTES                                  
