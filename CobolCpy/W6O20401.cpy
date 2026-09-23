000100 01  MOD-W6O20401.                                                        
000200*                                 MOD-COPYTEXT                            
000300*                                 FÖR W6O204                              
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDKR-IN          PIC X(2).                                    
000900*                                 MFS BEHANDLING AV INPUTFÄLT             
001000     03 MOD-IDKR-UT          PIC 9(5).                                    
001100*                                 KONTROLLRAPPORT NUMMER                  
001200     03 MOD-IDKOLLINR-IN     PIC X(2).                                    
001300*                                 MFS BEHANDLING AV INPUTFÄLT             
001400     03 MOD-IDKOLLINR-UT     PIC X(5).                                    
001500*                                 KOLLINUMMER                             
001600     03 MOD-IDKOLLI-ENTER    PIC X(5).                                    
001700*                                 KOLLINUMMER                             
001800     03 MOD-IDKOLLI-NEXT     PIC X(5).                                    
001900*                                 KOLLINUMMER                             
002000     03 MOD-IDARTNR          PIC Z(8)9.                                   
002100*                                 ARTIKELNUMMER                           
002200     03 MOD-BEART            PIC X(25).                                   
002300*                                 ARTIKELBENÄMNING                        
002400     03 MOD-KDKRSTA          PIC X.                                       
002500*                                 KONTROLLRAPPORT STATUS                  
002600     03 MOD-TIREGDAT         PIC 9(6).                                    
002700*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
002800     03 MOD-IDLEVNR          PIC X(5).                                    
002900*                                 LEVERANTÖRNUMMER                        
003000     03 MOD-IDLEVG           PIC Z(4)9.                                   
003100*                                 LEVERANTÖRS GODSADRESS NUMMER           
003200     03 MOD-BELEV            PIC X(35).                                   
003300*                                 LEVERANTÖRSNAMN                         
003400     03 MOD-RAD              OCCURS 7 TIMES.                              
003500        05 MOD-IDKOLLI       PIC Z(4)9.                                   
003600*                                 KOLLINUMMER                             
003700        05 MOD-VKKOLLIB      PIC Z(4)9.9.                                 
003800*                                 KOLLI-VIKT-BRUTTO                       
003900        05 MOD-DIKOLLIL      PIC Z(3)9.                                   
004000*                                 KOLLI-LÄNGD                             
004100        05 MOD-DIKOLLIB      PIC Z(2)9.                                   
004200*                                 KOLLI-BREDD                             
004300        05 MOD-DIKOLLIH      PIC Z(2)9.                                   
004400*                                 KOLLI-HÖJD                              
004500     03 MOD-KDKOLLI-ATTR     PIC X(2).                                    
004600*                                 MFS ATTRIBUTFÄLT                        
004700     03 MOD-KDKOLLI          PIC X(8).                                    
004800*                                 KOLLIKOD                                
004900     03 MOD-IDKOLLI-IN-ATTR  PIC X(2).                                    
005000*                                 MFS ATTRIBUTFÄLT                        
005100     03 MOD-IDKOLLI-IN       PIC Z(4)9.                                   
005200*                                 KOLLINUMMER                             
005300     03 MOD-VKKOLLIB-IN-ATTR PIC X(2).                                    
005400*                                 MFS ATTRIBUTFÄLT                        
005500     03 MOD-VKKOLLIB-IN      PIC Z(4)9.9.                                 
005600*                                 KOLLI-VIKT-BRUTTO                       
005700     03 MOD-DIKOLLIL-IN-ATTR PIC X(2).                                    
005800*                                 MFS ATTRIBUTFÄLT                        
005900     03 MOD-DIKOLLIL-IN      PIC Z(3)9.                                   
006000*                                 KOLLI-LÄNGD                             
006100     03 MOD-DIKOLLIB-IN-ATTR PIC X(2).                                    
006200*                                 MFS ATTRIBUTFÄLT                        
006300     03 MOD-DIKOLLIB-IN      PIC Z(2)9.                                   
006400*                                 KOLLI-BREDD                             
006500     03 MOD-DIKOLLIH-IN-ATTR PIC X(2).                                    
006600*                                 MFS ATTRIBUTFÄLT                        
006700     03 MOD-DIKOLLIH-IN      PIC Z(2)9.                                   
006800*                                 KOLLI-HÖJD                              
006900     03 MOD-KDCMD-IN-ATTR    PIC X(2).                                    
007000*                                 MFS ATTRIBUTFÄLT                        
007100     03 MOD-KDCMD-IN         PIC X.                                       
007200*                                 RAD-UPPDATERINGSKOMMANDO                
007300*                                  BLANK  = INGENTING                     
007400*                                  D , B  = DELETE                        
007500*                                  R , Ä  = REPLACE                       
007600*                                  I,N,A  = INSERT                        
007700*                                  S , V  = SELECT                        
007800*                                  P , P  = PRINT                         
007900*                                  C , K  = COPY                          
008000     03 MOD-KDPERSON-ATTR    PIC X(2).                                    
008100*                                 MFS ATTRIBUTFÄLT                        
008200     03 MOD-KDPERSON         PIC Z(2)9.                                   
008300*                                 PERSONKOD                               
008400     03 MOD-BEKRPACK-ATTR    PIC X(2).                                    
008500*                                 MFS ATTRIBUTFÄLT                        
008600     03 MOD-BEKRPACK         PIC X(25).                                   
008700*                                 ANSVARIG FÖR PACKNING                   
008800*                                                                         
008900     03 MOD-KVKRPACK-IN-ATTR PIC X(2).                                    
009000*                                 MFS ATTRIBUTFÄLT                        
009100     03 MOD-KVKRPACK-IN      PIC X(4).                                    
009200*                                 PACKNINGSTID                            
009300     03 MOD-KVKRPACK-UT      PIC Z9.9.                                    
009400*                                 PACKNINGSTID                            
009500     03 MOD-TIKRPACK         PIC 9(6).                                    
009600*                                 PACKNINGSDATUM                          
009700     03 MOD-TEMFSINF         PIC X(55).                                   
009800*                                 INFORMATIONSMEDDELANDE                  
009900*** END OF VILMAII-COPY LENGTH= 456 BYTES                                 
