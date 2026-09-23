000100 01  BIP-W461001.                                                         
000200*                                 BIPACKNING TILL NOAC PT-001             
000300     03 BIP-IDPTYP           PIC X(3).                                    
000400*                                 POSTTYP                                 
000500     03 BIP-IDDC             PIC X(2).                                    
000600*                                 IDENTIFIERARE LAGER                     
000700     03 BIP-IDDISTR          PIC S9(5)           COMP-3.                  
000800*                                 DISTRIKTNUMMER                          
000900     03 BIP-IDKUNDNR         PIC S9(7)           COMP-3.                  
001000*                                 KUNDNUMMER                              
001100     03 BIP-IDORDNR          PIC S9(7)           COMP-3.                  
001200*                                 ORDERNR             IDORDNR-002         
001300     03 BIP-KDORDKL          PIC S9              COMP-3.                  
001400*                                 ORDERKLASS                              
001500     03 BIP-IDARTNR          PIC S9(9)           COMP-3.                  
001600*                                 ARTIKELNUMMER                           
001700     03 BIP-REKSIFFR         PIC S9              COMP-3.                  
001800*                                 KONTROLLSIFFRA                          
001900     03 BIP-BERADREF         PIC X(10).                                   
002000*                                 KUNDENS RADREFERENS                     
002100     03 BIP-IDRONR           PIC S9(7)           COMP-3.                  
002200*                                 RESTORDERNUMMER      IDRONR-002         
002300     03 BIP-TIRODAT          PIC S9(7)           COMP-3.                  
002400*                                 RESTORDERDATUM         (≈≈MMDD)         
002500     03 BIP-BEVOLREF         PIC X(10).                                   
002600*                                 VOLVO REFERENS                          
002700     03 BIP-KVLEVART         PIC S9(7)           COMP-3.                  
002800*                                 LEVERERAT ANTAL STYCK                   
002900     03 BIP-KDFAKTYP         PIC X.                                       
003000*                                 FAKTURATYP                              
003100     03 BIP-KDRESTR          PIC S9(3)           COMP-3.                  
003200*                                 RESTRIKTIONSKOD                         
003300     03 BIP-TIAAMMDD         PIC S9(7)           COMP-3.                  
003400*                                 ≈R - M≈NAD - DAG  (≈≈MMDD)              
003500     03 BIP-TIKLOCK          PIC S9(9)           COMP-3.                  
003600*                                 KLOCKSLAG (TTMMSSTH)                    
003700*** END COPY W461001     LENGTH=67                                        
