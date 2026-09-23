000100 01  DLEV-W411DLEV.                                                       
000200*                                 LÄNKAREA TILL W411DLEV -                
000300*                                 KONTROLL OM DIREKTLEVERANSART           
000400     03 DLEV-INDATA.                                                      
000500        05 DLEV-IDDISTR-IN   PIC S9(5)           COMP-3.                  
000600*                                 DISTRIKTNUMMER                          
000700        05 DLEV-IDKUNDNR-IN  PIC S9(7)           COMP-3.                  
000800*                                 KUNDNUMMER                              
000900        05 DLEV-KDORDKL-IN   PIC S9              COMP-3.                  
001000*                                 ORDERKLASS                              
001100        05 DLEV-IDARTNR-IN   PIC S9(9)           COMP-3.                  
001200*                                 ARTIKELNUMMER                           
001300        05 DLEV-IDLEVNR-IN   PIC X(5).                                    
001400*                                 LEVERANTÖRNUMMER                        
001500        05 DLEV-KVBEART-Q-IN PIC S9(7)           COMP-3.                  
001600*                                 BESTÄLLT KVANTANPASSAT ANTAL            
001700        05 DLEV-REDIRLEV-IN  PIC S9V9(2)         COMP-3.                  
001800*                                 DIREKTLEVERANSANDEL                     
001900        05 DLEV-IDDC-IN      PIC X(2).                                    
002000*                                 IDENTIFIERARE LAGER                     
002100        05 DLEV-IDDC-ORD-IN  PIC X(2).                                    
002200*                                 IDENTIFIERARE LAGER                     
002300        05 DLEV-IDKAMPRF-IN  PIC S9(7)           COMP-3.                  
002400*                                 KAMPANJREFERENS                         
002500        05 DLEV-KDTPOTYP-IN  PIC S9              COMP-3.                  
002600*                                 TYP AV TIDPLANERAD ORDER                
002700        05 DLEV-KDUART-IN    PIC X.                                       
002800*                                 UNDANTAGSARTIKEL                        
002900        05 DLEV-FLFORBI-IN   PIC X.                                       
003000*                                 FÖRBIORDERFLAGGA                        
003100        05 DLEV-FLRESTN-IN   PIC X.                                       
003200*                                 RESTNOTERING ?                          
003300        05 DLEV-FLREFILL-IN  PIC X.                                       
003400*                                 REFILLARTIKEL                           
003500        05 DLEV-KDORDING-IN  PIC S9              COMP-3.                  
003600*                                 UPPDATERING ORDERINGÅNG                 
003700        05 DLEV-IDDC-CLEAR-IN                                             
003800                             OCCURS 99 TIMES                              
003900                             PIC X(2).                                    
004000*                                 LAGERPRIORITERING VID                   
004100*                                 ORDERCLEARING                           
004200        05 DLEV-KDCALL       PIC S9(3)           COMP-3.                  
004300*                                 ANROPSTYP                               
004400        05 DLEV-IDKUNDRF-IN  PIC X(10).                                   
004500*                                 KUNDENS REFERENS (ORDERID)              
004600     03 DLEV-UTDATA.                                                      
004700        05 DLEV-KDORDBEK-UT  PIC 9(2).                                    
004800*                                 ORDERBEKRÄFTELSEKOD                     
004900        05 DLEV-IDLEVNR-UT   PIC X(5).                                    
005000*                                 LEVERANTÖRNUMMER                        
005100        05 DLEV-FLRESTN-UT   PIC X.                                       
005200*                                 RESTNOTERING ?                          
005300        05 DLEV-FLSDCLEV-UT  PIC X.                                       
005400*                                 LEVERANSSTYRNING SDC                    
005500        05 DLEV-IDDC-UT      PIC X(2).                                    
005600*                                 IDENTIFIERARE LAGER                     
005700        05 DLEV-KDORDSTA-UT  PIC X(2).                                    
005800*                                 VOLVOORDERSTATUS                        
005900        05 DLEV-KDVIA-UT     PIC X(2).                                    
006000*                                 KOD FöR LEVERANS VIA                    
006100        05 DLEV-KVDAGAR-DIFF-UT                                           
006200                             PIC S9(3)           COMP-3.                  
006300*                                 TRANSPORT DAY DIFF. (+/- CONTRA         
006400*                                  CDC)                                   
006500        05 DLEV-TISKEPPN-DDC-UT                                           
006600                             PIC S9(7)           COMP-3.                  
006700*                                 SKEPPNINGSDATUM DLEV (ÅÅMMDD)           
006800        05 DLEV-KDOI-UT      PIC X(2).                                    
006900*                                 ORDERINGÅNGSTYP                         
007000        05 DLEV-KVLS-DLEV-UT PIC S9(7)           COMP-3.                  
007100*                                 LAGERSALDO HOS DIREKTLEVENATÖR          
007200        05 DLEV-TIINLMOT-UT  PIC S9(7)           COMP-3.                  
007300*                                 MOTTAGNINGSDATUM   (ÅÅMMDD)             
007400        05 DLEV-TIREGDAT-UT  PIC S9(7)           COMP-3.                  
007500*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
007600     03 DLEV-CLEARGROUP.                                                  
007700*                                 CLEARINGAREA FÖR ORDERINGÅNG            
007800        05 DLEV-CLEARAREA    OCCURS 7 TIMES.                              
007900*                                 CLEARINGAREA FÖR ORDERINGÅNG            
008000           07 DLEV-IDDC-CLEAR                                             
008100                             PIC X(2).                                    
008200*                                 LAGERPRIORITERING VID                   
008300*                                 ORDERCLEARING                           
008400           07 DLEV-FLLF      PIC X.                                       
008500*                                 ARTIKEL LAGERFÖRES                      
008600           07 DLEV-FLCLEAR   PIC X.                                       
008700*                                 ORDERRAD CLEAR FLAGGA                   
008800*** END OF VILMAII-COPY LENGTH= 311 BYTES                                 
