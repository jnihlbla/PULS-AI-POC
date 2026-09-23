000100 01  W425SU7-CTX.                                                         
000200*                                 FAKTURAHUVUD2: IDKUNDRF                 
000300     03 IDPTYP               PIC X(3).                                    
000400*                                 POSTTYP                                 
000500     03 IDDISTR              PIC 9(4).                                    
000600*                                 DISTRIKTNUMMER                          
000700     03 IDKUNDNR             PIC 9(6).                                    
000800*                                 KUNDNUMMER                              
000900     03 IDSUPPL              PIC 9(5).                                    
001000*                                 LEVERANSNR TILL ÅTERFÖRSÄLJARE          
001100     03 IDFAKT               PIC 9(7).                                    
001200*                                 FAKTURANUMMER                           
001300     03 IDORDNR-002          PIC 9(7).                                    
001400*                                 ORDERNR             IDORDNR-002         
001500     03 KDORDER              PIC 9.                                       
001600*                                 ORDERKOD                                
001700     03 KVRAD-UPPD           PIC 9(5).                                    
001800*                                 ANTAL ORDERRADER      KVRAD-002         
001900     03 KVKOLLIO-002         PIC 9(3).                                    
002000*                                                    KVKOLLIO-002         
002100*                                 ANTAL KOLLI PER ORDER                   
002200     03 VLORD                PIC 9(4)V9(3).                               
002300*                                 ORDER-VOLYM NETTO (M3)                  
002400     03 VKORDBTO             PIC 9(7).                                    
002500*                                 ORDERVIKT BRUTTO (KG)                   
002600     03 KDFAKTYP             PIC X.                                       
002700*                                 FAKTURATYP                              
002800     03 IDPRODNR             PIC 9(7).                                    
002900*                                 PRODUKTIONSNUMMER                       
003000     03 KDREFNOT             PIC X(2).                                    
003100*                                 FAKTURA NOTERINGAR                      
003200     03 TIAAMMDD             PIC 9(6).                                    
003300*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
003400     03 TIKLOCK              PIC 9(8).                                    
003500*                                 KLOCKSLAG (TTMMSSTH)                    
003600     03 FILLERX35            PIC X(35).                                   
003700*** END OF VILMAII-COPY LENGTH= 114 BYTES                                 
