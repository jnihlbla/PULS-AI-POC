000100 01  W61141.                                                              
000200*                                 URVAL FRÅN W6FILA                       
000300*                                 FÖR EV RENSNING                         
000400*                                                               .         
000500*                                 TRANSACTIONS FROM W6FILA                
000600*                                 FOR POSSIBLE DELETION                   
000700*                                                               .         
000800     03 IDDC                 PIC X(2).                                    
000900*                                 IDENTIFIERARE LAGER                     
001000*                                 WAREHOUSE IDENTIFIER                    
001100     03 TIREGDAT             PIC S9(7)           COMP-3.                  
001200*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
001300*                                 REGISTRATION DATE (YYMMDD)              
001400     03 TIKLOCK              PIC S9(9)           COMP-3.                  
001500*                                 KLOCKSLAG (TTMMSSTH)                    
001600*                                 TIME OF DAY (HHMMSSTH)                  
001700     03 IDLOPNRM             PIC S9(9)           COMP-3.                  
001800*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
001900*                                 (0VVDLLLLK)                             
002000*                                 SERIAL NO RECEIVING REPORT              
002100*                                 (0WWDLLLLC)                             
002200     03 IDRADNR              PIC S9(5)           COMP-3.                  
002300*                                 RADNUMMER                               
002400*                                 LINE NO                                 
002500     03 IDPGM                PIC X(8).                                    
002600*                                 PROGRAM IDENTITET                       
002700*                                 PROGRAM INTENTITY                       
002800     03 IDSEKVNR             PIC S9(3)           COMP-3.                  
002900*                                 GENERELLT SEKVENSNUMMER                 
003000*                                 GENERAL SEQUENCE NUMBER                 
003100     03 IDCPYTXT.                                                         
003200*                                 COPYTEXT IDENTITET                      
003300*                                 IDENTITY OF A COPYTEXT                  
003400        05 CT-IDSYSTEM       PIC X(4).                                    
003500*                                 SKAPANDE SYSTEMNUMMER                   
003600*                                 GENERATING SYSTEM NUMBER                
003700        05 CT-IDPTYP         PIC X(3).                                    
003800*                                 POSTTYP                                 
003900*                                 RECORD TYPE                             
004000        05 CT-IDVTYP         PIC X.                                       
004100*                                 POSTTYPSVERSION                         
004200*                                 RECORD TYPE VERSION                     
004300     03 KDINLPRIO            PIC S9(3)           COMP-3.                  
004400*                                 PRIORITETSGRUPP                         
004500*                                 PRIORITY GROUP                          
004600     03 KDINLSTA             PIC X(3).                                    
004700*                                 SYSTEMSTATUS INLEVERANS                 
004800*                                 SYSTEM STATUS RECEIVING                 
004900     03 KDINLUPF             PIC X(4).                                    
005000*                                 UPPFÖLJNINGSSTATUS INLEVERANS           
005100*                                 FOLLOW-UP STATUS RECEIVING              
005200     03 KDINLUPF-NXT         PIC X(4).                                    
005300*                                 NÄSTA UPPFÖLJNINGSSTATUS INLEVE         
005400*                                 RANS                                    
005500*                                 NEXT FOLLOW-UP STATUS RECEIVING         
005600     03 KVINLART             PIC S9(7)           COMP-3.                  
005700*                                 ANTAL I PARTIRAD                        
005800*                                 QTY/LINE IN A LOT                       
005900     03 PRARTSTD             PIC S9(7)V9(2)      COMP-3.                  
006000*                                 ARTIKELSTANDARDPRIS                     
006100*                                 STANDARD PRICE                          
006200     03 KVKOLLI              PIC S9(5)           COMP-3.                  
006300*                                 ANTAL KOLLI                             
006400*                                 NBR OF CASES                            
006500     03 FLINLI               PIC X.                                       
006600*                                 INLAGD RAD, PARTI ELLER KOLLI           
006700*                                 STORED  LINE                            
006800*** END OF VILMAII-COPY LENGTH= 63 BYTES                                  
