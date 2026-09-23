000100 01  W479060.                                                             
000200*                                 FAKTURERADE KOLLIN                      
000300     03 IDPTYP               PIC X(3).                                    
000400*                                 POSTTYP                                 
000500     03 IDDC                 PIC X(2).                                    
000600*                                 IDENTIFIERARE LAGER                     
000700     03 IDDISTR              PIC S9(5)           COMP-3.                  
000800*                                 DISTRIKTNUMMER                          
000900     03 IDKUNDNR             PIC S9(7)           COMP-3.                  
001000*                                 KUNDNUMMER                              
001100     03 IDKUNDRF             PIC X(10).                                   
001200*                                 KUNDENS REFERENS (ORDERID)              
001300     03 IDPRODNR             PIC S9(7)           COMP-3.                  
001400*                                 PRODUKTIONSNUMMER                       
001500     03 IDKOLLI              PIC S9(5)           COMP-3.                  
001600*                                 KOLLINUMMER                             
001700     03 KDORDKL              PIC S9              COMP-3.                  
001800*                                 ORDERKLASS                              
001900     03 KDFRAKT              PIC S9(3)           COMP-3.                  
002000*                                 FRAKTSÄTT DC TILL KUND                  
002100     03 IDTRPTNR             PIC S9(3)           COMP-3.                  
002200*                                 TRANSPORTIDENTITET                      
002300     03 ADFLGEO              PIC X(3).                                    
002400*                                 GEOGRAFISKT OMRÅDE FÄRDIGLAGER          
002500     03 ADFLOMR              PIC S9(3)           COMP-3.                  
002600*                                 LASTNINGSOMR/STÄLL FÄRDIGLAGER          
002700     03 ADRUTNIV             PIC S9(3)           COMP-3.                  
002800*                                 RUTA/NIVÅ I FÄRDIGLAGRET                
002900     03 ADVMODUL             PIC S9(3)           COMP-3.                  
003000*                                 VÄNSTER-MODUL                           
003100     03 KDKOLLI              PIC X(8).                                    
003200*                                 KOLLIKOD                                
003300     03 DIKOLLIL             PIC S9(5)           COMP-3.                  
003400*                                 KOLLI-LÄNGD                             
003500     03 DIKOLLIB             PIC S9(3)           COMP-3.                  
003600*                                 KOLLI-BREDD                             
003700     03 DIKOLLIH             PIC S9(3)           COMP-3.                  
003800*                                 KOLLI-HÖJD                              
003900     03 KDEMBTYP             PIC S9              COMP-3.                  
004000*                                 EMBALLAGETYP                            
004100     03 KVORDRAD             PIC S9(5)           COMP-3.                  
004200*                                 ANTAL ORDERRADER                        
004300     03 VKORDBTO-KOLLI       PIC S9(6)V9(1)      COMP-3.                  
004400*                                 ORDERVIKT BRUTTO PER KOLLI              
004500     03 VKORDNTO-KOLLI       PIC S9(6)V9(1)      COMP-3.                  
004600*                                 ORDERVIKT NETTO PER KOLLI               
004700     03 VLORDBTO-KOLLI       PIC S9(4)V9(3)      COMP-3.                  
004800*                                 ORDERVOLYM BRUTTO KOLLI                 
004900     03 SUORDV-KOLLI         PIC S9(9)V9(2)      COMP-3.                  
005000*                                 VARUVÄRDE PER KOLLI                     
005100     03 SUORDV-LOC           PIC S9(9)V9(2)      COMP-3.                  
005200*                                 ORDERVÄRDE SLUTKUNDPRIS                 
005300*                                 I LOKAL VALUTA                          
005400     03 SUORDV-LOCPREL       PIC S9(9)V9(2)      COMP-3.                  
005500*                                 ORDERVÄRDE PREL SLUT-                   
005600*                                 KUNDPRIS, LOKAL VALUTA                  
005700     03 KDVALISO             PIC X(3).                                    
005800*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
005900     03 FLDIRLEV             PIC X.                                       
006000*                                 DIREKTLEVERANS ?                        
006100     03 IDPLOCK              PIC S9(7)           COMP-3.                  
006200*                                 PLOCKARE                                
006300*** END OF VILMAII-COPY LENGTH= 100 BYTES                                 
