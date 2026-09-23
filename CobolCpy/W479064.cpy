000100 01  W479064.                                                             
000200*                                  FAKTURERADE RADER                      
000300     03 IDPTYP               PIC X(3).                                    
000400*                                 POSTTYP                                 
000500     03 IDDC-RET             PIC X(2).                                    
000600*                                 MOTTAGANDE LAGER FÖR RETURER            
000700     03 IDDISTR              PIC S9(5)           COMP-3.                  
000800*                                 DISTRIKTNUMMER                          
000900     03 IDFTG                PIC 9(2).                                    
001000*                                 FÖRETAGSID EKONOM REDOVISNING           
001100     03 IDKUNDNR             PIC S9(7)           COMP-3.                  
001200*                                 KUNDNUMMER                              
001300     03 IDKUNDRF             PIC X(10).                                   
001400*                                 KUNDENS REFERENS (ORDERID)              
001500     03 IDRADNR-KO           PIC S9(5)           COMP-3.                  
001600*                                 RADNUMMER KUNDORDER                     
001700     03 KVLEVART2            PIC S9(7)           COMP-3.                  
001800*                                 FAKTISKT LEVERERAT ANTAL I KOLL         
001900*                                 IT                                      
002000     03 IDPRODNR             PIC S9(7)           COMP-3.                  
002100*                                 PRODUKTIONSNUMMER                       
002200     03 IDKOLLI              PIC S9(5)           COMP-3.                  
002300*                                 KOLLINUMMER                             
002400     03 TIPACKN              PIC S9(7)           COMP-3.                  
002500*                                 PACKNINGSDATUM         (ÅÅMMDD)         
002600     03 TIFAKT               PIC S9(7)           COMP-3.                  
002700*                                 FAKTURERINGSDATUM (ÅÅMMDD)              
002800     03 TILASTN              PIC S9(7)           COMP-3.                  
002900*                                 LASTNINGSDATUM         (ÅÅMMDD)         
003000     03 TIFAKT-BILLIT        PIC S9(7)           COMP-3.                  
003100*                                 FAKTURERINGSDATUM (ÅÅMMDD)              
003200     03 TILASTN-BILLIT       PIC S9(7)           COMP-3.                  
003300*                                 LASTNINGSDATUM         (ÅÅMMDD)         
003400     03 IDFAKT               PIC S9(7)           COMP-3.                  
003500*                                 FAKTURANUMMER                           
003600     03 KDKOLLI              PIC X(8).                                    
003700*                                 KOLLIKOD                                
003800     03 KVORDRAD             PIC S9(5)           COMP-3.                  
003900*                                 ANTAL ORDERRADER                        
004000     03 SUORDV-KOLLI         PIC S9(9)V9(2)      COMP-3.                  
004100*                                 VARUVÄRDE PER KOLLI                     
004200     03 SUORDV-LOC           PIC S9(9)V9(2)      COMP-3.                  
004300*                                 ORDERVÄRDE SLUTKUNDPRIS                 
004400*                                 I LOKAL VALUTA                          
004500     03 SUORDV-LOCPREL       PIC S9(9)V9(2)      COMP-3.                  
004600*                                 ORDERVÄRDE PREL SLUT-                   
004700*                                 KUNDPRIS, LOKAL VALUTA                  
004800     03 KDVALISO             PIC X(3).                                    
004900*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
005000     03 VKORDBTO-KOLLI       PIC S9(6)V9(1)      COMP-3.                  
005100*                                 ORDERVIKT BRUTTO PER KOLLI              
005200     03 VKORDNTO-KOLLI       PIC S9(6)V9(1)      COMP-3.                  
005300*                                 ORDERVIKT NETTO PER KOLLI               
005400     03 IDSHIPM              PIC 9(7).                                    
005500*                                 SKEPPNINGSNUMMER                        
005600     03 TISKEPPN             PIC S9(7)           COMP-3.                  
005700*                                 SKEPPNINGSDATUM  (ÅÅMMDD)               
005800*** END OF VILMAII-COPY LENGTH= 113 BYTES                                 
