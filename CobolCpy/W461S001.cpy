000100 01  BIP-W461S001.                                                        
000200*                                 SAMMANSLAGNING AV SORTDEL OCH           
000300*                                 BIPACKNINGS INFO TILL NOAC              
000400     03 BIP-SOR0-IDDISTR     PIC S9(5)           COMP-3.                  
000500*                                 DISTRIKTNUMMER                          
000600     03 BIP-SOR0-IDKUNDNR    PIC S9(7)           COMP-3.                  
000700*                                 KUNDNUMMER                              
000800     03 BIP-SOR0-IDRONR      PIC S9(7)           COMP-3.                  
000900*                                 RESTORDERNUMMER      IDRONR-002         
001000     03 BIP-SOR0-TIRODAT     PIC S9(7)           COMP-3.                  
001100*                                 RESTORDERDATUM         (≈≈MMDD)         
001200     03 BIP-SOR0-IDPTYP      PIC X(3).                                    
001300*                                 POSTTYP                                 
001400     03 BIP-SOR0-IDLOPNR     PIC S9(5)           COMP-3.                  
001500*                                 L÷PNUMMER          IDLOPNR-002          
001600     03 BIP-W461001.                                                      
001700*                                 BIPACKNING TILL NOAC PT-001             
001800        05 BIP-IDPTYP        PIC X(3).                                    
001900*                                 POSTTYP                                 
002000        05 BIP-IDDC          PIC X(2).                                    
002100*                                 IDENTIFIERARE LAGER                     
002200        05 BIP-IDDISTR       PIC S9(5)           COMP-3.                  
002300*                                 DISTRIKTNUMMER                          
002400        05 BIP-IDKUNDNR      PIC S9(7)           COMP-3.                  
002500*                                 KUNDNUMMER                              
002600        05 BIP-IDORDNR       PIC S9(7)           COMP-3.                  
002700*                                 ORDERNR             IDORDNR-002         
002800        05 BIP-KDORDKL       PIC S9              COMP-3.                  
002900*                                 ORDERKLASS                              
003000        05 BIP-IDARTNR       PIC S9(9)           COMP-3.                  
003100*                                 ARTIKELNUMMER                           
003200        05 BIP-REKSIFFR      PIC S9              COMP-3.                  
003300*                                 KONTROLLSIFFRA                          
003400        05 BIP-BERADREF      PIC X(10).                                   
003500*                                 KUNDENS RADREFERENS                     
003600        05 BIP-IDRONR        PIC S9(7)           COMP-3.                  
003700*                                 RESTORDERNUMMER      IDRONR-002         
003800        05 BIP-TIRODAT       PIC S9(7)           COMP-3.                  
003900*                                 RESTORDERDATUM         (≈≈MMDD)         
004000        05 BIP-BEVOLREF      PIC X(10).                                   
004100*                                 VOLVO REFERENS                          
004200        05 BIP-KVLEVART      PIC S9(7)           COMP-3.                  
004300*                                 LEVERERAT ANTAL STYCK                   
004400        05 BIP-KDFAKTYP      PIC X.                                       
004500*                                 FAKTURATYP                              
004600        05 BIP-KDRESTR       PIC S9(3)           COMP-3.                  
004700*                                 RESTRIKTIONSKOD                         
004800        05 BIP-TIAAMMDD      PIC S9(7)           COMP-3.                  
004900*                                 ≈R - M≈NAD - DAG  (≈≈MMDD)              
005000        05 BIP-TIKLOCK       PIC S9(9)           COMP-3.                  
005100*                                 KLOCKSLAG (TTMMSSTH)                    
005200*** END COPY W461S001    LENGTH=88                                        
