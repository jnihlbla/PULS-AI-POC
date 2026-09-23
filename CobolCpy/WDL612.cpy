000100 01  ORD-WDL612.                                                          
000200*                                 INLEVERANS HISTORIK SDC                 
000300*                                 ORDER INFORMATION                       
000400*                                 FYSISK NYCKEL: WDL612KY                 
000500*                                  (DAREGDAT + TIREGTID)                  
000600*                                 SÖKBEGREPP IDLEVNR                      
000700*                                            IDLOPNRM                     
000800     03 ORD-DAREGDAT         PIC 9(8).                                    
000900*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
001000*                                 REGISTRATION DATE (YYYYMMDD)            
001100     03 ORD-TIREGTID         PIC S9(7)           COMP-3.                  
001200*                                 REGISTRERINGSTID                        
001300*                                 GENERAL REGISTRATION TIME               
001400     03 ORD-IDDC             PIC X(2).                                    
001500*                                 IDENTIFIERARE LAGER                     
001600*                                 WAREHOUSE IDENTIFIER                    
001700     03 ORD-IDKUNDRF         PIC X(10).                                   
001800*                                 KUNDENS REFERENS (ORDERID)              
001900*                                 CUSTOMER REFERENCE (ORDER ID)           
002000     03 ORD-IDLEVNR          PIC X(5).                                    
002100*                                 LEVERANTÖRNUMMER                        
002200*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
002300     03 ORD-IDLOPNRM         PIC S9(9)           COMP-3.                  
002400*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
002500*                                 (0VVDLLLLK)                             
002600*                                 SERIAL NO RECEIVING REPORT              
002700*                                 (0WWDLLLLC)                             
002800     03 ORD-KVBEART          PIC S9(7)           COMP-3.                  
002900*                                 BESTÄLLT ANTAL STYCKEN                  
003000*                                 ORDERED QUANTITY                        
003100     03 ORD-KVAVIS           PIC S9(7)           COMP-3.                  
003200*                                 AVISERAT ANTAL                          
003300*                                 QUANTITY NOTIFIED                       
003400     03 ORD-TIBERANK         PIC 9(6).                                    
003500*                                 BERÄKNAD ANKOMSTDATUM                   
003600*                                 ESTIMATED RECEIVING DATE                
003700*** END OF VILMAII-COPY LENGTH= 48 BYTES                                  
