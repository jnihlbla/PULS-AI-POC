000100 01  MOD-W6O21301.                                                        
000200*                                 MOD-COPYTEXT                            
000300*                                 FÖR W6O213                              
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600*                                 SCREEN NUMBER                           
000700     03 MOD-TEMFSFEL         PIC X(40).                                   
000800*                                 MFS FELMEDDELANDE                       
000900*                                 MFS ERROR MESSAGE                       
001000     03 MOD-IDARTNR-IN       PIC X(2).                                    
001100*                                 MFS BEHANDLING AV INPUTFÄLT             
001200*                                 MFS DISPOSITION OF INPUT FIELD          
001300     03 MOD-IDARTNR-UT       PIC X(9).                                    
001400*                                 ARTIKELNUMMER                           
001500*                                 PART NUMBER                             
001600     03 MOD-IDLEVNR-IN       PIC X(2).                                    
001700*                                 MFS BEHANDLING AV INPUTFÄLT             
001800*                                 MFS DISPOSITION OF INPUT FIELD          
001900     03 MOD-IDLEVNR-UT       PIC X(5).                                    
002000*                                 LEVERANTÖRNUMMER                        
002100*                                 SUPPLIER NUMBER                         
002200     03 MOD-IDKVAINFI        PIC X(2).                                    
002300*                                 MFS BEHANDLING AV INPUTFÄLT             
002400*                                 MFS DISPOSITION OF INPUT FIELD          
002500     03 MOD-IDKVAINFU        PIC X(2).                                    
002600*                                 RADNR FÖR KVALITETSKONTROLLTEXT         
002700*                                 LINENO FOR QUALITY CONTROL TEXT         
002800     03 MOD-TIREGDAT-IN      PIC X(2).                                    
002900*                                 MFS BEHANDLING AV INPUTFÄLT             
003000*                                 MFS DISPOSITION OF INPUT FIELD          
003100     03 MOD-TIREGDAT-UT      PIC X(6).                                    
003200*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
003300*                                 REGISTRATION DATE (YYMMDD)              
003400     03 MOD-KDKVAINF-IN      PIC X(2).                                    
003500*                                 MFS BEHANDLING AV INPUTFÄLT             
003600*                                 MFS DISPOSITION OF INPUT FIELD          
003700     03 MOD-KDKVAINF-UT      PIC X.                                       
003800*                                 TYP AV KVAL.INFO FÖR ARTIKEL            
003900*                                 TYPE OF QUAL.INFO. FOR PART             
004000     03 MOD-IDKVAINF-NEXT    PIC 9(2).                                    
004100*                                 RADNR FÖR KVALITETSKONTROLLTEXT         
004200*                                 LINENO FOR QUALITY CONTROL TEXT         
004300     03 MOD-IDKVAINF-ENTER   PIC 9(2).                                    
004400*                                 RADNR FÖR KVALITETSKONTROLLTEXT         
004500*                                 LINENO FOR QUALITY CONTROL TEXT         
004600     03 MOD-BEART            PIC X(25).                                   
004700*                                 ARTIKELBENÄMNING                        
004800*                                 PART DESCRIPTION                        
004900     03 MOD-BELEV            PIC X(35).                                   
005000*                                 LEVERANTÖRSNAMN                         
005100*                                 SUPPLIER NAME                           
005200     03 MOD-FLKVASAK-ATTR    PIC X(2).                                    
005300*                                 MFS ATTRIBUTFÄLT                        
005400     03 MOD-FLKVASAK         PIC X(2).                                    
005500*                                 MFS BEHANDLING AV INPUTFÄLT             
005600*                                 MFS DISPOSITION OF INPUT FIELD          
005700     03 MOD-TIKVASAK         PIC 9(6).                                    
005800*                                 DATUM KVALITETSSÄKRAD                   
005900*                                 DATE QUALITY ASSURED                    
006000     03 MOD-FLUPG-ATTR       PIC X(2).                                    
006100*                                 MFS ATTRIBUTFÄLT                        
006200     03 MOD-FLUPG            PIC X(2).                                    
006300*                                 MFS BEHANDLING AV INPUTFÄLT             
006400*                                 MFS DISPOSITION OF INPUT FIELD          
006500     03 MOD-TIUPG            PIC 9(6).                                    
006600*                                 TID NÄR UTFALLSPROV GJORTS              
006700*                                 DATE FOR ENDED QUALITY CONTROL          
006800     03 MOD-KDCMD-LEV-ATTR   PIC X(2).                                    
006900*                                 MFS ATTRIBUTFÄLT                        
007000     03 MOD-KDCMD-LEV        PIC X(2).                                    
007100*                                 MFS BEHANDLING AV INPUTFÄLT             
007200*                                 MFS DISPOSITION OF INPUT FIELD          
007300     03 MOD-RAD              OCCURS 5 TIMES.                              
007400        05 MOD-IDKVAINF      PIC Z9.                                      
007500*                                 RADNR FÖR KVALITETSKONTROLLTEXT         
007600*                                 LINENO FOR QUALITY CONTROL TEXT         
007700        05 MOD-IDPROVPL-PRI  PIC X.                                       
007800*                                 PROVTAGNINGSPLAN PRIM.KONTROLL          
007900*                                 SAMPLE PLAN PRIMARY INSPECTION          
008000        05 MOD-KVSKPLOT-PRI  PIC 9.                                       
008100        05 MOD-BEANST        PIC X(25).                                   
008200*                                 ANSTÄLLDS NAMN                          
008300*                                 NAME OF EMPLOYED                        
008400        05 MOD-IDTFN         PIC X(20).                                   
008500*                                 TELEFONNUMMER EXTERNT                   
008600*                                 TELEPHONE NUMBER  EXTERNAL              
008700     03 MOD-IDKVAINF-IN-ATTR PIC X(2).                                    
008800*                                 MFS ATTRIBUTFÄLT                        
008900     03 MOD-IDKVAINF-IN      PIC X(2).                                    
009000*                                 MFS BEHANDLING AV INPUTFÄLT             
009100*                                 MFS DISPOSITION OF INPUT FIELD          
009200     03 MOD-IDPROVPL-PRI-IN-ATTR                                          
009300                             PIC X(2).                                    
009400*                                 MFS ATTRIBUTFÄLT                        
009500     03 MOD-IDPROVPL-PRI-IN  PIC X(2).                                    
009600*                                 MFS BEHANDLING AV INPUTFÄLT             
009700*                                 MFS DISPOSITION OF INPUT FIELD          
009800     03 MOD-KVSKPLOT-PRI-IN-ATTR                                          
009900                             PIC X(2).                                    
010000*                                 MFS ATTRIBUTFÄLT                        
010100     03 MOD-KVSKPLOT-PRI-IN  PIC X(2).                                    
010200*                                 MFS BEHANDLING AV INPUTFÄLT             
010300*                                 MFS DISPOSITION OF INPUT FIELD          
010400     03 MOD-BEANST-IN-ATTR   PIC X(2).                                    
010500*                                 MFS ATTRIBUTFÄLT                        
010600     03 MOD-BEANST-IN        PIC X(25).                                   
010700*                                 ANSTÄLLDS NAMN                          
010800*                                 NAME OF EMPLOYED                        
010900     03 MOD-IDTFN-IN-ATTR    PIC X(2).                                    
011000*                                 MFS ATTRIBUTFÄLT                        
011100     03 MOD-IDTFN-IN         PIC X(20).                                   
011200*                                 TELEFONNUMMER EXTERNT                   
011300*                                 TELEPHONE NUMBER  EXTERNAL              
011400     03 MOD-KDCMD-SPEC-ATTR  PIC X(2).                                    
011500*                                 MFS ATTRIBUTFÄLT                        
011600     03 MOD-KDCMD-SPEC       PIC X(2).                                    
011700*                                 MFS BEHANDLING AV INPUTFÄLT             
011800*                                 MFS DISPOSITION OF INPUT FIELD          
011900     03 MOD-TEMFSINF         PIC X(55).                                   
012000*                                 INFORMATIONSMEDDELANDE                  
012100*                                 INFORMATION MESSAGE                     
012200*** END OF VILMAII-COPY LENGTH= 530 BYTES                                 
