000100 01  MOD-W6O20301.                                                        
000200*                                 MOD-COPYTEXT                            
000300*                                 FÖR W6O203                              
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDKR-IN          PIC X(2).                                    
000900*                                 MFS BEHANDLING AV INPUTFÄLT             
001000     03 MOD-IDKR-UT          PIC 9(5).                                    
001100*                                 KONTROLLRAPPORT NUMMER                  
001200     03 MOD-IDARTNR          PIC Z(8)9.                                   
001300*                                 ARTIKELNUMMER                           
001400     03 MOD-ARTIKELTEXT      PIC X(24).                                   
001500     03 MOD-KDKRSTA          PIC X.                                       
001600*                                 KONTROLLRAPPORT STATUS                  
001700     03 MOD-TIREGDAT         PIC 9(6).                                    
001800*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
001900     03 MOD-IDANSK           PIC Z(2)9.                                   
002000*                                 ANSKAFFARNUMMER                         
002100     03 MOD-IDLEVNR          PIC X(5).                                    
002200*                                 LEVERANTÖRNUMMER                        
002300     03 MOD-IDLEVG           PIC Z(4)9.                                   
002400*                                 LEVERANTÖRS GODSADRESS NUMMER           
002500     03 MOD-LEVTEXT          PIC X(30).                                   
002600     03 MOD-FLKRLFEL-ATTR    PIC X(2).                                    
002700*                                 MFS ATTRIBUTFÄLT                        
002800     03 MOD-FLKRLFEL         PIC X(2).                                    
002900*                                 MFS BEHANDLING AV INPUTFÄLT             
003000     03 MOD-KDKRATG-ATTR     PIC X(2).                                    
003100*                                 MFS ATTRIBUTFÄLT                        
003200     03 MOD-KDKRATG          PIC X(2).                                    
003300*                                 MFS BEHANDLING AV INPUTFÄLT             
003400     03 MOD-FLINKANS-ATTR    PIC X(2).                                    
003500*                                 MFS ATTRIBUTFÄLT                        
003600     03 MOD-FLINKANS         PIC X(2).                                    
003700*                                 MFS BEHANDLING AV INPUTFÄLT             
003800     03 MOD-ADATTENT-ATTR    PIC X(2).                                    
003900*                                 MFS ATTRIBUTFÄLT                        
004000     03 MOD-ADATTENT         PIC X(40).                                   
004100*                                 ATTENTIONADRESS                         
004200     03 MOD-FLKRTOVIR-ATTR   PIC X(2).                                    
004300*                                 MFS ATTRIBUTFÄLT                        
004400     03 MOD-FLKRTOVIR        PIC X(2).                                    
004500*                                 MFS BEHANDLING AV INPUTFÄLT             
004600     03 MOD-FLEJKNTRL-ATTR   PIC X(2).                                    
004700*                                 MFS ATTRIBUTFÄLT                        
004800     03 MOD-FLEJKNTRL        PIC X.                                       
004900*                                 KVALITET KONTROLL FLAGGA                
005000     03 MOD-KVARBTID-IN-ATTR PIC X(2).                                    
005100*                                 MFS ATTRIBUTFÄLT                        
005200     03 MOD-KVARBTID-IN      PIC X(4).                                    
005300*                                 ANTAL MANTIMMAR                         
005400     03 MOD-KVARBTID-UT      PIC Z9.9.                                    
005500*                                 ANTAL MANTIMMAR                         
005600     03 MOD-TEKRSPEC-ATID-ATTR                                            
005700                             PIC X(2).                                    
005800*                                 MFS ATTRIBUTFÄLT                        
005900     03 MOD-TEKRSPEC-ATID    PIC X(30).                                   
006000*                                 SPECIFIKATION ARBETSTID                 
006100     03 MOD-SUOMK-IN-ATTR    PIC X(2).                                    
006200*                                 MFS ATTRIBUTFÄLT                        
006300     03 MOD-SUOMK-IN         PIC X(7).                                    
006400*                                 SUMMA OMKOSTNADER                       
006500     03 MOD-SUOMK-UT         PIC Z(6)9.                                   
006600*                                 SUMMA OMKOSTNADER                       
006700     03 MOD-TEKRSPEC-OMK-ATTR                                             
006800                             PIC X(2).                                    
006900*                                 MFS ATTRIBUTFÄLT                        
007000     03 MOD-TEKRSPEC-OMK     PIC X(30).                                   
007100*                                 SPECIFIKATION OMKOSTNADER               
007200     03 MOD-SUMAT-IN-ATTR    PIC X(2).                                    
007300*                                 MFS ATTRIBUTFÄLT                        
007400     03 MOD-SUMAT-IN         PIC X(10).                                   
007500*                                 MATERIALKOSTNAD                         
007600     03 MOD-SUMAT-UT         PIC Z(6)9.9(2).                              
007700*                                 MATERIALKOSTNAD                         
007800     03 MOD-TEKRSPEC-MAT-ATTR                                             
007900                             PIC X(2).                                    
008000*                                 MFS ATTRIBUTFÄLT                        
008100     03 MOD-TEKRSPEC-MAT     PIC X(30).                                   
008200*                                 SPECIFIKATION MATERIALKOSTNAD           
008300     03 MOD-FLKROMK-ATTR     PIC X(2).                                    
008400*                                 MFS ATTRIBUTFÄLT                        
008500     03 MOD-FLKROMK          PIC X(2).                                    
008600*                                 MFS BEHANDLING AV INPUTFÄLT             
008700     03 MOD-FLARBDEB-ATTR    PIC X(2).                                    
008800*                                 MFS ATTRIBUTFÄLT                        
008900     03 MOD-FLARBDEB         PIC X(2).                                    
009000*                                 MFS BEHANDLING AV INPUTFÄLT             
009100     03 MOD-FLKRLIM-ATTR     PIC X(2).                                    
009200*                                 MFS ATTRIBUTFÄLT                        
009300     03 MOD-FLKRLIM          PIC X(2).                                    
009400*                                 MFS BEHANDLING AV INPUTFÄLT             
009500     03 MOD-SUKRLIM          PIC Z(3).Z(2).                               
009600*                                 SUMMAGRÄNS LÅGT VÄRDE                   
009700     03 MOD-KDPERSON-ATTR    PIC X(2).                                    
009800*                                 MFS ATTRIBUTFÄLT                        
009900     03 MOD-KDPERSON         PIC Z(2)9.                                   
010000*                                 PERSONKOD                               
010100     03 MOD-BEKRANS          PIC X(25).                                   
010200*                                 ANSVARIG                                
010300*                                                                         
010400     03 MOD-IDKRATLF-ATTR    PIC X(2).                                    
010500*                                 MFS ATTRIBUTFÄLT                        
010600     03 MOD-IDKRATLF         PIC X(20).                                   
010700*                                 TELEFON TILL ANSVARIG                   
010800*                                                                         
010900     03 MOD-FLKRGODK-ATTR    PIC X(2).                                    
011000*                                 MFS ATTRIBUTFÄLT                        
011100     03 MOD-FLKRGODK         PIC X(2).                                    
011200*                                 MFS BEHANDLING AV INPUTFÄLT             
011300     03 MOD-FAX-TLX-ATTR     PIC X(2).                                    
011400*                                 MFS ATTRIBUTFÄLT                        
011500     03 MOD-FAX-TLX          PIC X(3).                                    
011600     03 MOD-KDFAX-ATTR       PIC X(2).                                    
011700*                                 MFS ATTRIBUTFÄLT                        
011800     03 MOD-KDFAX            PIC X(2).                                    
011900*                                 MFS BEHANDLING AV INPUTFÄLT             
012000     03 MOD-IDLEVFAX-ATTR    PIC X(2).                                    
012100*                                 MFS ATTRIBUTFÄLT                        
012200     03 MOD-IDLEVFAX         PIC X(16).                                   
012300*                                 TELEFAXNUMMER TILL LEVERANTÖR           
012400     03 MOD-FLANN-ATTR       PIC X(2).                                    
012500*                                 MFS ATTRIBUTFÄLT                        
012600     03 MOD-FLANN            PIC X(2).                                    
012700*                                 MFS BEHANDLING AV INPUTFÄLT             
012800     03 MOD-KDMEMO-ATTR      PIC X(2).                                    
012900*                                 MFS ATTRIBUTFÄLT                        
013000     03 MOD-KDMEMO           PIC X(2).                                    
013100*                                 MFS BEHANDLING AV INPUTFÄLT             
013200     03 MOD-IDMAIL-ATTR      PIC X(2).                                    
013300*                                 MFS ATTRIBUTFÄLT                        
013400     03 MOD-IDMAIL           PIC X(60).                                   
013500*                                 MAIL ADRESS                             
013600     03 MOD-TEMFSINF         PIC X(55).                                   
013700*                                 INFORMATIONSMEDDELANDE                  
013800*** END OF VILMAII-COPY LENGTH= 565 BYTES                                 
