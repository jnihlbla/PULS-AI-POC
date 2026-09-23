000100 01  W479A11.                                                             
000200*                                 PACKUNDERLAG, HISTORIK                  
000300*                                 KOLLI                                   
000400     03 IDPTYP               PIC X(3).                                    
000500*                                 POSTTYP                                 
000600     03 IDDISTR              PIC S9(5)           COMP-3.                  
000700*                                 DISTRIKTNUMMER                          
000800     03 IDKUNDNR             PIC S9(7)           COMP-3.                  
000900*                                 KUNDNUMMER                              
001000     03 IDPRODNR             PIC S9(7)           COMP-3.                  
001100*                                 PRODUKTIONSNUMMER                       
001200     03 IDDC                 PIC X(2).                                    
001300*                                 IDENTIFIERARE LAGER                     
001400     03 IDKOLLI              PIC S9(5)           COMP-3.                  
001500*                                 KOLLINUMMER                             
001600     03 IDPURAD              PIC S9(5)           COMP-3.                  
001700*                                 RADNUMMER PÅ PACKUNDERLAG               
001800     03 IDKUNDRF             PIC X(10).                                   
001900*                                 KUNDENS REFERENS (ORDERID)              
002000     03 IDARTNR              PIC S9(9)           COMP-3.                  
002100*                                 ARTIKELNUMMER                           
002200     03 ADFLGEO              PIC X(3).                                    
002300*                                 GEOGRAFISKT OMRÅDE FÄRDIGLAGER          
002400     03 ADFLOMR              PIC S9(3)           COMP-3.                  
002500*                                 LASTNINGSOMR/STÄLL FÄRDIGLAGER          
002600     03 ADRUTNIV             PIC S9(3)           COMP-3.                  
002700*                                 RUTA/NIVÅ I FÄRDIGLAGRET                
002800     03 ADVMODUL             PIC S9(3)           COMP-3.                  
002900*                                 VÄNSTER-MODUL                           
003000     03 TIPACKN              PIC S9(7)           COMP-3.                  
003100*                                 PACKNINGSDATUM         (ÅÅMMDD)         
003200     03 TIFAKT               PIC S9(7)           COMP-3.                  
003300*                                 FAKTURERINGSDATUM (ÅÅMMDD)              
003400     03 TILASTN              PIC S9(7)           COMP-3.                  
003500*                                 LASTNINGSDATUM         (ÅÅMMDD)         
003600     03 IDFAKT               PIC S9(7)           COMP-3.                  
003700*                                 FAKTURANUMMER                           
003800     03 KVORDRAD             PIC S9(5)           COMP-3.                  
003900*                                 ANTAL ORDERRADER                        
004000     03 VKORDBTO-KOLLI       PIC S9(6)V9(1)      COMP-3.                  
004100*                                 ORDERVIKT BRUTTO PER KOLLI              
004200     03 VLORDBTO-KOLLI       PIC S9(4)V9(3)      COMP-3.                  
004300*                                 ORDERVOLYM BRUTTO KOLLI                 
004400     03 IDPLOCK              PIC S9(7)           COMP-3.                  
004500*                                 PLOCKARE                                
004600     03 KDKOLLI              PIC X(8).                                    
004700*                                 KOLLIKOD                                
004800*** END OF VILMAII-COPY LENGTH= 85 BYTES                                  
