000100* GENERATION OF COBOL HOST STRUCTURE FROM TQ1TRPUP-TAB                    
000200  01 TQ1TRPUP.                                                            
000300*              TQ1TRPUP TRANSPORTS FOLLOWUP                               
000400   03 TILASTN          PIC S9(7) COMP-3.                                  
000500*              LASTNINGSDATUM         (ÅÅMMDD)                            
000600   03 IDPRODNR         PIC S9(7) COMP-3.                                  
000700*              PRODUKTIONSNUMMER                                          
000800   03 IDKOLLI          PIC S9(5) COMP-3.                                  
000900*              KOLLINUMMER                                                
001000   03 IDDC             PIC X(2).                                          
001100*              IDENTIFIERARE LAGER                                        
001200   03 IDDISTR          PIC S9(5) COMP-3.                                  
001300*              DISTRIKTNUMMER                                             
001400   03 IDKUNDNR         PIC S9(7) COMP-3.                                  
001500*              KUNDNUMMER                                                 
001600   03 KDFAKTYP         PIC X(1).                                          
001700*              FAKTURATYP                                                 
001800   03 IDFAKT           PIC S9(7) COMP-3.                                  
001900*              FAKTURANUMMER                                              
002000   03 TIFAKT           PIC S9(7) COMP-3.                                  
002100*              FAKTURERINGSDATUM (ÅÅMMDD)                                 
002200   03 TIFAKTID         PIC S9(7) COMP-3.                                  
002300*              FAKTURERINGSTID                                            
002400   03 KDORDKL          PIC S9(1) COMP-3.                                  
002500*              ORDERKLASS                                                 
002600   03 KDFRAKT          PIC S9(3) COMP-3.                                  
002700*              FRAKTSÄTT DC TILL KUND                                     
002800   03 FLDIRLEV         PIC X(1).                                          
002900*              DIREKTLEVERANS ?                                           
003000   03 IDLEVNR          PIC X(5).                                          
003100*              LEVERANTÖRNUMMER                                           
003200   03 KDVIA            PIC X(2).                                          
003300*              KOD FöR LEVERANS VIA                                       
003400   03 DASUPREF         PIC X(8).                                          
003500*              SÄNDNINGSDATUM DIREKTLEVERANTÖR                            
003600   03 TISUPTID         PIC S9(5) COMP-3.                                  
003700*              SÄNDNINGSTID DIREKTLEVERANTÖR                              
003800   03 TIUTSKR          PIC S9(7) COMP-3.                                  
003900*              UTSKRIFTDATUM  (ÅÅMMDD)                                    
004000   03 TIUTSTID         PIC S9(7) COMP-3.                                  
004100*              UTSKRIFTSTID (TTMMSS)                                      
004200   03 TILASTID         PIC S9(7) COMP-3.                                  
004300*              LASTNINGSTID                                               
004400   03 IDTRPTNR         PIC S9(3) COMP-3.                                  
004500*              TRANSPORTIDENTITET                                         
004600   03 IDLBBET          PIC X(12).                                         
004700*              LASTBÄRARBETECKNING                                        
004800   03 IDSHIPM          PIC X(7).                                          
004900*              SKEPPNINGSNUMMER                                           
005000   03 IDLASTN          PIC S9(7) COMP-3.                                  
005100*              LASTNINGSNUMMER                                            
005200   03 DIKOLLIL         PIC S9(5) COMP-3.                                  
005300*              KOLLI-LÄNGD                                                
005400   03 DIKOLLIB         PIC S9(3) COMP-3.                                  
005500*              KOLLI-BREDD                                                
005600   03 DIKOLLIH         PIC S9(3) COMP-3.                                  
005700*              KOLLI-HÖJD                                                 
005800   03 KDKOLLI          PIC X(8).                                          
005900*              KOLLIKOD                                                   
006000   03 KDFARLIG-KOLLI   PIC S9(1) COMP-3.                                  
006100*              KOD FÖR FARLIGT GODS I KOLLI                               
006200   03 KVFLAMP-KOLLI    PIC S9(2)V9(1) COMP-3.                             
006300*              KOLLITS FLAMPUNKT                                          
006400   03 SUEQFG           PIC S9(3)V9(4) COMP-3.                             
006500*              EQ-VÄRDE FARLIGT GODS                                      
006600   03 KVFALRAD         PIC S9(5) COMP-3.                                  
006700*              ANTAL RADER MED FARLIGT GODS                               
006800   03 KVORDRAD         PIC S9(5) COMP-3.                                  
006900*              ANTAL ORDERRADER                                           
007000   03 SUORDV-KOLLI     PIC S9(9)V9(2) COMP-3.                             
007100*              VARUVÄRDE PER KOLLI                                        
007200   03 VKORDNTO-KOLLI   PIC S9(6)V9(1) COMP-3.                             
007300*              ORDERVIKT NETTO PER KOLLI                                  
007400   03 VKORDBTO-KOLLI   PIC S9(6)V9(1) COMP-3.                             
007500*              ORDERVIKT BRUTTO PER KOLLI                                 
007600   03 VLORDBTO-KOLLI   PIC S9(4)V9(3) COMP-3.                             
007700*              ORDERVOLYM BRUTTO KOLLI                                    
007800   03 FLLDCKND         PIC X(1).                                          
007900*              FL LDC-KUND                                                
008000   03 IDLANDX2         PIC X(2).                                          
008100*              2-STÄLLIG LANDSBETECKNINGSKOD                              
008200*                                                                         
008300*** END OF VILMAII-COPY LENGTH= 141 OLD LENGTH=                           
