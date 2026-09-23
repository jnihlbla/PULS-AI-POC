000100 01  MOD-W5O21801.                                                        
000200*                                 MOD-COPYTEXT FÖR W50218                 
000300*                                 PRIME COUNT SELECTION                   
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-KDEKHHT-IN       PIC X(2).                                    
000900*                                 MFS BEHANDLING AV INPUTFÄLT             
001000     03 MOD-KDEKHHT-UT       PIC X(3).                                    
001100*                                 EKONOMISK HUVUDHÄNDELSE                 
001200     03 MOD-KDEKSHT-IN       PIC X(2).                                    
001300*                                 MFS BEHANDLING AV INPUTFÄLT             
001400     03 MOD-KDEKSHT-UT       PIC X(3).                                    
001500*                                 EKONOMISK SUBHÄNDELSE                   
001600     03 MOD-KDEKNIVA-IN      PIC X(2).                                    
001700*                                 MFS BEHANDLING AV INPUTFÄLT             
001800     03 MOD-KDEKNIVA-UT      PIC X(5).                                    
001900*                                 EKONOMISK HÄNDELSENIVÅ                  
002000     03 MOD-IDSYSMOT-IN      PIC X(2).                                    
002100*                                 MFS BEHANDLING AV INPUTFÄLT             
002200     03 MOD-IDSYSMOT-UT      PIC X(6).                                    
002300*                                 PULS MOTTAGANDE SYSTEMNAMN              
002400     03 MOD-IDPTYP-IN        PIC X(2).                                    
002500*                                 MFS BEHANDLING AV INPUTFÄLT             
002600     03 MOD-IDPTYP-UT        PIC X(3).                                    
002700*                                 POSTTYP                                 
002800     03 MOD-IDFTG-UT         PIC 9(2).                                    
002900*                                 FÖRETAGSID EKONOM REDOVISNING           
003000     03 MOD-IDSEKVNR-ATTR    PIC X(2).                                    
003100*                                 MFS ATTRIBUTFÄLT                        
003200     03 MOD-IDSEKVNR         PIC 9(3).                                    
003300*                                 GENERELLT SEKVENSNUMMER                 
003400     03 MOD-BEEKHHT-ATTR     PIC X(2).                                    
003500*                                 MFS ATTRIBUTFÄLT                        
003600     03 MOD-BEEKHHT          PIC X(25).                                   
003700*                                 BESKR. EKONOMISK HUVUDHÄNDELSE          
003800     03 MOD-BEEKSHT-ATTR     PIC X(2).                                    
003900*                                 MFS ATTRIBUTFÄLT                        
004000     03 MOD-BEEKSHT          PIC X(25).                                   
004100*                                 BESKR. EKONOMISK SUBHÄNDELSE            
004200     03 MOD-KDEKNIVA-ATTR    PIC X(2).                                    
004300*                                 MFS ATTRIBUTFÄLT                        
004400     03 MOD-KDEKNIVA         PIC X(5).                                    
004500*                                 EKONOMISK HÄNDELSENIVÅ                  
004600     03 MOD-IDSYSMOT-ATTR    PIC X(2).                                    
004700*                                 MFS ATTRIBUTFÄLT                        
004800     03 MOD-IDSYSMOT         PIC X(6).                                    
004900*                                 PULS MOTTAGANDE SYSTEMNAMN              
005000     03 MOD-IDPTYP-ATTR      PIC X(2).                                    
005100*                                 MFS ATTRIBUTFÄLT                        
005200     03 MOD-IDPTYP           PIC X(3).                                    
005300*                                 POSTTYP                                 
005400     03 MOD-IDKONTO-ATTR     PIC X(2).                                    
005500*                                 MFS ATTRIBUTFÄLT                        
005600     03 MOD-IDKONTO          PIC Z(9)9.                                   
005700*                                 KONTO                                   
005800     03 MOD-IDANALYS-ATTR    PIC X(2).                                    
005900*                                 MFS ATTRIBUTFÄLT                        
006000     03 MOD-IDANALYS         PIC X(12).                                   
006100*                                 ANALYSNUMMER                            
006200     03 MOD-IDPRCTR-ATTR     PIC X(2).                                    
006300*                                 MFS ATTRIBUTFÄLT                        
006400     03 MOD-IDPRCTR          PIC X(10).                                   
006500*                                 PROFIT CENTER                           
006600     03 MOD-IDKST-ATTR       PIC X(2).                                    
006700*                                 MFS ATTRIBUTFÄLT                        
006800     03 MOD-IDKST            PIC X(10).                                   
006900*                                 KOSTNADSSTÄLLE                          
007000     03 MOD-KDDOKTYP-ATTR    PIC X(2).                                    
007100*                                 MFS ATTRIBUTFÄLT                        
007200     03 MOD-KDDOKTYP         PIC X(2).                                    
007300*                                 DOCUMENT TYPE                           
007400     03 MOD-KDANALYS-ATTR    PIC X(2).                                    
007500*                                 MFS ATTRIBUTFÄLT                        
007600     03 MOD-KDANALYS         PIC X.                                       
007700*                                 ANALYSIS CODE                           
007800     03 MOD-KDPOST-ATTR      PIC X(2).                                    
007900*                                 MFS ATTRIBUTFÄLT                        
008000     03 MOD-KDPOST           PIC X(2).                                    
008100*                                 POSTING KEY                             
008200     03 MOD-KDTECKEN-ATTR    PIC X(2).                                    
008300*                                 MFS ATTRIBUTFÄLT                        
008400     03 MOD-KDTECKEN         PIC X.                                       
008500*                                 PLUS ELLER MINUS (+ -)                  
008600     03 MOD-FLPRSEGM-ATTR    PIC X(2).                                    
008700*                                 MFS ATTRIBUTFÄLT                        
008800     03 MOD-FLPRSEGM         PIC X.                                       
008900*                                 IND. PROFITABILITY SEGMENT              
009000     03 MOD-FLALLOC-ATTR     PIC X(2).                                    
009100*                                 MFS ATTRIBUTFÄLT                        
009200     03 MOD-FLALLOC          PIC X.                                       
009300*                                 IND. ALLOCATION FIELD VALUE             
009400     03 MOD-TEMFSINF         PIC X(55).                                   
009500*                                 INFORMATIONSMEDDELANDE                  
009600*** END OF VILMAII-COPY LENGTH= 280 BYTES                                 
