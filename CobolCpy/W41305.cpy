000100 01  W41305-CTX.                                                          
000200*                                 WOPS - OUTPUT FROM W41305-PGM           
000300     03 IDORDER              PIC S9(7)           COMP-3.                  
000400*                                 VOLVO PARTS ORDERNUMMER                 
000500*                                 VOLVO PARTS ORDER NUMBER                
000600     03 IDDISTR              PIC S9(5)           COMP-3.                  
000700*                                 DISTRIKTNUMMER                          
000800*                                 DISTRICT NUMBER                         
000900     03 IDKUNDNR             PIC S9(7)           COMP-3.                  
001000*                                 KUNDNUMMER                              
001100*                                 CUSTOMER NO                             
001200     03 IDKUNDRF             PIC X(10).                                   
001300*                                 KUNDENS REFERENS (ORDERID)              
001400*                                 CUSTOMER REFERENCE (ORDER ID)           
001500     03 IDDC                 PIC X(2).                                    
001600*                                 IDENTIFIERARE LAGER                     
001700*                                 WAREHOUSE IDENTIFIER                    
001800     03 KDFRAKT              PIC S9(3)           COMP-3.                  
001900*                                 FRAKTSÄTT DC TILL KUND                  
002000*                                 FREIGHT CODE                            
002100     03 ADLAGOMR             PIC S9(3)           COMP-3.                  
002200*                                 LAGEROMRÅDE                             
002300*                                 AREA                                    
002400     03 IDPRC.                                                            
002500*                                 PRODUKTIONSKANAL                        
002600*                                 PRODUCTION CHANNEL                      
002700        05 IDPRCBAS          PIC X(3).                                    
002800*                                 PRC-BAS                                 
002900*                                 PRC-BASIC                               
003000        05 IDPRCVAR          PIC X.                                       
003100*                                 PRC-VARIANT                             
003200*                                 PRC-VARIANT                             
003300     03 KVANTART             PIC S9(5)           COMP-3.                  
003400*                                 ANTAL-ARTIKLAR                          
003500*                                 QUANTITY PARTS                          
003600     03 KVRADER              PIC S9(5)           COMP-3.                  
003700*                                 ANTAL RADER                             
003800*                                 NUMBER OF LINES                         
003900     03 KVSEMBRA             PIC S9(3)           COMP-3.                  
004000*                                 ANTAL SPECIALEMBALLAGERADER             
004100*                                 NUMBER OF SPECIAL PACKING LINES         
004200     03 SUHANTTI             PIC S9(7)           COMP-3.                  
004300*                                 SUMMA HANTERINGSKOD TID                 
004400*                                 TOTAL PIECEWORK TIME                    
004500     03 SUORDV               PIC S9(9)V9(2)      COMP-3.                  
004600*                                 SUMMA ORDERVÄRDE                        
004700*                                 TOTAL ORDER VALUE                       
004800     03 VKORDNTO             PIC S9(6)V9(1)      COMP-3.                  
004900*                                 ORDERVIKT NETTO (KG)                    
005000*                                 WEIGHT PER ORDER NETTO (KG)             
005100     03 VLORDNTO             PIC S9(4)V9(3)      COMP-3.                  
005200*                                 ORDERVOLYM NETTO (M3)                   
005300*                                 NET VOLUME PER ORDER (M3)               
005400     03 KDORDKL              PIC S9              COMP-3.                  
005500*                                 ORDERKLASS                              
005600*                                 ORDER CLASS                             
005700*** END OF VILMAII-COPY LENGTH= 58 BYTES                                  
