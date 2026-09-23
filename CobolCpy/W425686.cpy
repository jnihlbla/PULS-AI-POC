000100 01  W425686.                                                             
000200*                                 ORDERHUVUD I FAKTURA TILL               
000300*                                 VR-SYSTEM                               
000400*                                                                         
000500     03 IDPTYP               PIC X(3).                                    
000600*                                 POSTTYP                                 
000700     03 IDDISTR              PIC S9(5)           COMP-3.                  
000800*                                 DISTRIKTNUMMER                          
000900     03 IDKUNDNR             PIC S9(7)           COMP-3.                  
001000*                                 KUNDNUMMER                              
001100     03 IDDC                 PIC X(2).                                    
001200*                                 IDENTIFIERARE LAGER                     
001300     03 IDFAKT               PIC S9(7)           COMP-3.                  
001400*                                 FAKTURANUMMER                           
001500     03 KDFAKTYP             PIC X.                                       
001600*                                 FAKTURATYP                              
001700     03 IDKUNDRF             PIC X(10).                                   
001800*                                 KUNDENS REFERENS (ORDERID)              
001900     03 KDORDKL              PIC S9              COMP-3.                  
002000*                                 ORDERKLASS                              
002100     03 KVRAD-UPD            PIC S9(7)           COMP-3.                  
002200*                                 ANTAL ORDERRADER                        
002300     03 KVKOLLIO             PIC S9(5)           COMP-3.                  
002400*                                 ANTAL KOLLI PER ORDER                   
002500     03 VLORDBTO             PIC S9(4)V9(3)      COMP-3.                  
002600*                                 ORDERVOLYM BRUTTO (M3)                  
002700     03 VKORDBTO             PIC S9(6)V9(1)      COMP-3.                  
002800*                                 ORDERVIKT BRUTTO (KG)                   
002900     03 IDPRODNR             PIC S9(7)           COMP-3.                  
003000*                                 PRODUKTIONSNUMMER                       
003100     03 KDREFNOT             PIC X(2).                                    
003200*                                 FAKTURA NOTERINGAR                      
003300     03 TIAAMMDD             PIC S9(7)           COMP-3.                  
003400*                                 ≈R - M≈NAD - DAG  (≈≈MMDD)              
003500     03 TIKLOCK              PIC S9(9)           COMP-3.                  
003600*                                 KLOCKSLAG (TTMMSSTH)                    
003700*** END OF VILMAII-COPY LENGTH= 58 BYTES                                  
