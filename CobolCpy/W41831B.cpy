000100 01  W41831B.                                                             
000200*                                 KREDITPOST-RAD TILL W41831 OCH          
000300*                                 BILLIT                                  
000400     03 IDPTYP               PIC X(3).                                    
000500*                                 POSTTYP                                 
000600     03 IDLEVANM.                                                         
000700*                                 LEVERANSANMÄRKNINGSIDENTITET            
000800        05 IDDISTR           PIC S9(5)           COMP-3.                  
000900*                                 DISTRIKTNUMMER                          
001000        05 IDKUNDNR          PIC S9(7)           COMP-3.                  
001100*                                 KUNDNUMMER                              
001200        05 IDRAPPNR          PIC 9(7).                                    
001300*                                 RAPPORT NUMMER                          
001400     03 IDDC                 PIC X(2).                                    
001500*                                 IDENTIFIERARE LAGER                     
001600     03 IDDC-RET             PIC X(2).                                    
001700*                                 MOTTAGANDE LAGER FÖR RETURER            
001800     03 IDARTNR              PIC S9(9)           COMP-3.                  
001900*                                 ARTIKELNUMMER                           
002000     03 IDRADNR              PIC S9(5)           COMP-3.                  
002100*                                 RADNUMMER                               
002200     03 KDANMORS             PIC X(2).                                    
002300*                                 ORSAK TILL LEVERANSANMÄRKNING           
002400     03 DALEVANM             PIC 9(8).                                    
002500*                                 DATUM LEV.ANMÄRKNING(YYYYMMDD)          
002600     03 FLSOFT               PIC X.                                       
002700*                                 FLAGGA SOFTVARA                         
002800     03 FLLSBOK              PIC X.                                       
002900*                                 LAGERAVBOKNING                          
003000     03 FLINVUPD             PIC X.                                       
003100*                                 INVENTERINGSUPPDAT                      
003200     03 BEART-VIPS           PIC X(25).                                   
003300*                                 VIPS ARTIKELBENÄMNING                   
003400*                                 PÅ DEALERNS SPRÅK                       
003500     03 IDFAKREF             PIC S9(9)           COMP-3.                  
003600*                                 URSPRUNGLIGT FAKTURANUMMER              
003700     03 DAFAKREF             PIC 9(8).                                    
003800*                                 URSPRUNGLIGT FAKTURADATUM (ÅÅÅÅ         
003900*                                 MMDD)                                   
004000     03 KDAVVTYP             PIC S9              COMP-3.                  
004100*                                 AVVIKELSETYP                            
004200*                                 1=POSITIV.  2=NEGATIV                   
004300     03 KVLEVANM             PIC S9(7)           COMP-3.                  
004400*                                 LEVERANSANMÄRKNINGSANTAL                
004500     03 KVKREANT             PIC S9(7)           COMP-3.                  
004600*                                 KREDITERAT ANTAL                        
004700     03 PRARTBTO             PIC S9(7)V9(2)      COMP-3.                  
004800*                                 FÖRSÄLJNINGSPRIS BRUTTO (KR)            
004900     03 PRARTBTO-LOC         PIC S9(7)V9(2)      COMP-3.                  
005000*                                 PRIS I LOKAL VALUTA                     
005100     03 KDVAT                PIC X(2).                                    
005200*                                 MOMSKOD                                 
005300     03 KDVALISO             PIC X(3).                                    
005400*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
005500     03 FLFREE               PIC X.                                       
005600*                                 GRATISFATURA                            
005700     03 IDPARTNR             PIC X(9).                                    
005800*                                 PARTNERNUMMER                           
005900     03 IDSKYLT              PIC X(3).                                    
006000*                                 NATIONALITETSTECKEN                     
006100*                                 SPRÅKIDENTIFIKATION                     
006200     03 IDSTATNR             PIC S9(9)           COMP-3.                  
006300*                                 STATISTISKT NUMMER                      
006400*                                 1 = NORSKT                              
006500*                                 2 = ENGELSKT                            
006600*                                 3 = BELGISKT                            
006700*                                 4 = PERUANSKT                           
006800*                                 5 = SVENSKT                             
006900*                                 6 =                                     
007000     03 KDARTURS             PIC X(2).                                    
007100*                                 ARTIKELURSPRUNGSKOD                     
007200     03 VKART                PIC S9(7)           COMP-3.                  
007300*                                 ARTIKELVIKT (G)                         
007400     03 PRLANDCO-RAD         PIC S9(7)V9(2)      COMP-3.                  
007500*                                 LANDING COST                            
007600     03 PRFRAKT              PIC S9(7)V9(2)      COMP-3.                  
007700*                                 FRAKTKOSTNAD                            
007800     03 PRFOERS              PIC S9(7)V9(2)      COMP-3.                  
007900*                                 FÖRSÄKRINGSPREMIE                       
008000     03 PRLEGKST             PIC S9(7)V9(2)      COMP-3.                  
008100*                                 LEGALISERINSKOSTNAD                     
008200     03 IDUSER-ADM           PIC X(8).                                    
008300*                                 ANVÄNDAR-ID ADMINISTRATIV KONTR         
008400     03 BEANST               PIC X(25).                                   
008500*                                 ANSTÄLLDS NAMN                          
008600*** END OF VILMAII-COPY LENGTH= 181 BYTES                                 
