000100 01  W4795J.                                                              
000200*                                  FAKTURERADE RADER                      
000300     03 IDPTYP               PIC X(3).                                    
000400*                                 POSTTYP                                 
000500     03 IDDISTR              PIC S9(5)           COMP-3.                  
000600*                                 DISTRIKTNUMMER                          
000700     03 IDKUNDNR             PIC S9(7)           COMP-3.                  
000800*                                 KUNDNUMMER                              
000900     03 IDKUNDRF             PIC X(10).                                   
001000*                                 KUNDENS REFERENS (ORDERID)              
001100     03 IDRADNR-KO           PIC S9(5)           COMP-3.                  
001200*                                 RADNUMMER KUNDORDER                     
001300     03 KVLEVART2            PIC S9(7)           COMP-3.                  
001400*                                 FAKTISKT LEVERERAT ANTAL I KOLL         
001500*                                 IT                                      
001600     03 IDPRODNR             PIC S9(7)           COMP-3.                  
001700*                                 PRODUKTIONSNUMMER                       
001800     03 IDKOLLI              PIC S9(5)           COMP-3.                  
001900*                                 KOLLINUMMER                             
002000     03 TIPACKN              PIC S9(7)           COMP-3.                  
002100*                                 PACKNINGSDATUM         (ÅÅMMDD)         
002200     03 TIFAKT               PIC S9(7)           COMP-3.                  
002300*                                 FAKTURERINGSDATUM (ÅÅMMDD)              
002400     03 TILASTN              PIC S9(7)           COMP-3.                  
002500*                                 LASTNINGSDATUM         (ÅÅMMDD)         
002600     03 IDFAKT               PIC S9(7)           COMP-3.                  
002700*                                 FAKTURANUMMER                           
002800     03 KDKOLLI              PIC X(8).                                    
002900*                                 KOLLIKOD                                
003000     03 KVORDRAD             PIC S9(5)           COMP-3.                  
003100*                                 ANTAL ORDERRADER                        
003200     03 SUORDV-KOLLI         PIC S9(9)V9(2)      COMP-3.                  
003300*                                 VARUVÄRDE PER KOLLI                     
003400     03 SUORDV-LOC           PIC S9(9)V9(2)      COMP-3.                  
003500*                                 ORDERVÄRDE SLUTKUNDPRIS                 
003600*                                 I LOKAL VALUTA                          
003700     03 SUORDV-LOCPREL       PIC S9(9)V9(2)      COMP-3.                  
003800*                                 ORDERVÄRDE PREL SLUT-                   
003900*                                 KUNDPRIS, LOKAL VALUTA                  
004000     03 KDVALISO             PIC X(3).                                    
004100*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
004200     03 VKORDBTO-KOLLI       PIC S9(6)V9(1)      COMP-3.                  
004300*                                 ORDERVIKT BRUTTO PER KOLLI              
004400     03 VKORDNTO-KOLLI       PIC S9(6)V9(1)      COMP-3.                  
004500*                                 ORDERVIKT NETTO PER KOLLI               
004600*** END OF VILMAII-COPY LENGTH= 90 BYTES                                  
