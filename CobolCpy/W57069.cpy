000100 01  DIR-W57069.                                                          
000200*                                 LOGGDATA TILL DIRLEV-RAPPORT            
000300     03 DIR-IDARTNR          PIC S9(9)           COMP-3.                  
000400*                                 ARTIKELNUMMER                           
000500*                                 PART NUMBER                             
000600     03 DIR-IDVERGL          PIC X(10).                                   
000700*                                 VERIFIKATIONSID FÖR HUVUDBOKEN          
000800*                                 VERIFICATION IDENTITY FOR THE           
000900*                                 GENERAL LEDGER                          
001000     03 DIR-IDDC             PIC X(2).                                    
001100*                                 IDENTIFIERARE LAGER                     
001200*                                 WAREHOUSE IDENTIFIER                    
001300     03 DIR-IDDISTR          PIC S9(5)           COMP-3.                  
001400*                                 DISTRIKTNUMMER                          
001500*                                 DISTRICT NUMBER                         
001600     03 DIR-IDKUNDNR         PIC S9(7)           COMP-3.                  
001700*                                 KUNDNUMMER                              
001800*                                 CUSTOMER NO                             
001900     03 DIR-IDORDNR5         PIC S9(5)           COMP-3.                  
002000*                                 ORDERNUMMER                             
002100*                                 ORDER NUMBER                            
002200     03 DIR-KDPRODSL         PIC S9(3)           COMP-3.                  
002300*                                 PRODUKTSLAG                             
002400*                                 PRODUCT GROUP                           
002500     03 DIR-DAVERDAT         PIC 9(8).                                    
002600*                                 VERIFIKATIONSDATUM (ÅÅÅÅMMDD)           
002700*                                 VERIFICATION DATE (YYYYMMDD)            
002800     03 DIR-KDEKHHT          PIC X(3).                                    
002900*                                 EKONOMISK HUVUDHÄNDELSE                 
003000*                                 ECONOMIC MAIN EVENT                     
003100     03 DIR-KDEKSHT          PIC X(3).                                    
003200*                                 EKONOMISK SUBHÄNDELSE                   
003300*                                 ECONOMIC SUB EVENT                      
003400     03 DIR-KVANTAL          PIC S9(7)           COMP-3.                  
003500*                                 ANTAL                                   
003600*                                 NUMBER                                  
003700     03 DIR-PRARTSTD         PIC S9(7)V9(2)      COMP-3.                  
003800*                                 ARTIKELSTANDARDPRIS                     
003900*                                 STANDARD PRICE                          
004000     03 DIR-PRLANDCO         PIC S9(7)V9(2)      COMP-3.                  
004100*                                 LANDING COST                            
004200*                                 LANDING COST                            
004300     03 DIR-KDTRADP          PIC X(4).                                    
004400*                                 TRADING PARTNER                         
004500*                                 TRADING PARTNER                         
004600*** END OF VILMAII-COPY LENGTH= 61 BYTES                                  
