000100 01  MOD-W2O45501.                                                        
000200*                                 MOD-COPYTEXT FÖR W2045500               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDDC-IN          PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900     03 MOD-IDDC-UT          PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100     03 MOD-COPY-IDDC-ATTR   PIC X(2).                                    
001200*                                 MFS ATTRIBUTFÄLT                        
001300     03 MOD-COPY-IDDC        PIC X(2).                                    
001400*                                 IDENTIFIERARE LAGER                     
001500     03 MOD-KVOT-RULL12HF-UT PIC Z(6)9.                                   
001600*                                 ANTAL ORDERINGÅNG HÖGFREKVENTA          
001700     03 MOD-KVOT-RULL12HF-IN-ATTR                                         
001800                             PIC X(2).                                    
001900*                                 MFS ATTRIBUTFÄLT                        
002000     03 MOD-KVOT-RULL12HF-IN PIC Z(6)9.                                   
002100*                                 ANTAL ORDERINGÅNG HÖGFREKVENTA          
002200     03 MOD-KVVECKOR-FTL-UT  PIC Z(2)9.                                   
002300*                                 MAX VECKOR FÖR LÅGA LEDTIDER            
002400     03 MOD-KVVECKOR-FTL-IN-ATTR                                          
002500                             PIC X(2).                                    
002600*                                 MFS ATTRIBUTFÄLT                        
002700     03 MOD-KVVECKOR-FTL-IN  PIC Z(2)9.                                   
002800*                                 MAX VECKOR FÖR LÅGA LEDTIDER            
002900     03 MOD-KVVECKOR-FTM-UT  PIC Z(2)9.                                   
003000*                                 MAX VECKOR FÖR MEDEL LEDTIDER           
003100     03 MOD-KVVECKOR-FTM-IN-ATTR                                          
003200                             PIC X(2).                                    
003300*                                 MFS ATTRIBUTFÄLT                        
003400     03 MOD-KVVECKOR-FTM-IN  PIC Z(2)9.                                   
003500*                                 MAX VECKOR FÖR MEDEL LEDTIDER           
003600     03 MOD-KVVECKOR-FTH-UT  PIC Z(2)9.                                   
003700*                                 MAX VECKOR FÖR HÖGA LEDTIDER            
003800     03 MOD-KVVECKOR-FTH-IN-ATTR                                          
003900                             PIC X(2).                                    
004000*                                 MFS ATTRIBUTFÄLT                        
004100     03 MOD-KVVECKOR-FTH-IN  PIC Z(2)9.                                   
004200*                                 MAX VECKOR FÖR HÖGA LEDTIDER            
004300     03 MOD-IDUSER           PIC X(8).                                    
004400*                                 ANVÄNDARENS SÄKERHETS ID                
004500     03 MOD-TIUPPDAT         PIC 9(6).                                    
004600*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
004700     03 MOD-GRP              OCCURS 13 TIMES.                             
004800        05 MOD-KDANSKSEG     PIC 9(4).                                    
004900*                                 KOD FÖR ANSKAFFNINGSSEGMENT             
005000        05 MOD-BEPSEGM       PIC X(40).                                   
005100        05 MOD-IDREFTAB-LFL-UT                                            
005200                             PIC X.                                       
005300*                                 TAB FÖR LÅG FREKV & LÅG LEDTID          
005400        05 MOD-IDREFTAB-LFL-IN-ATTR                                       
005500                             PIC X(2).                                    
005600*                                 MFS ATTRIBUTFÄLT                        
005700        05 MOD-IDREFTAB-LFL-IN                                            
005800                             PIC X.                                       
005900*                                 TAB FÖR LÅG FREKV & LÅG LEDTID          
006000        05 MOD-IDREFTAB-LFM-UT                                            
006100                             PIC X.                                       
006200*                                 TAB FÖR LÅG FREKV & MED LEDTID          
006300        05 MOD-IDREFTAB-LFM-IN-ATTR                                       
006400                             PIC X(2).                                    
006500*                                 MFS ATTRIBUTFÄLT                        
006600        05 MOD-IDREFTAB-LFM-IN                                            
006700                             PIC X.                                       
006800*                                 TAB FÖR LÅG FREKV & MED LEDTID          
006900        05 MOD-IDREFTAB-LFH-UT                                            
007000                             PIC X.                                       
007100*                                 LÅG FREKVENTA OCH HÖGA LEDTIDER         
007200        05 MOD-IDREFTAB-LFH-IN-ATTR                                       
007300                             PIC X(2).                                    
007400*                                 MFS ATTRIBUTFÄLT                        
007500        05 MOD-IDREFTAB-LFH-IN                                            
007600                             PIC X.                                       
007700*                                 LÅG FREKVENTA OCH HÖGA LEDTIDER         
007800        05 MOD-IDREFTAB-LFXH-UT                                           
007900                             PIC X.                                       
008000*                                 HÖG FREKVENTA & X-HÖGA LEDTIDER         
008100        05 MOD-IDREFTAB-LFXH-IN-ATTR                                      
008200                             PIC X(2).                                    
008300*                                 MFS ATTRIBUTFÄLT                        
008400        05 MOD-IDREFTAB-LFXH-IN                                           
008500                             PIC X.                                       
008600*                                 HÖG FREKVENTA & X-HÖGA LEDTIDER         
008700        05 MOD-IDREFTAB-HFL-UT                                            
008800                             PIC X.                                       
008900*                                 HÖG FREKVENTA OCH LÅGA LEDTIDER         
009000        05 MOD-IDREFTAB-HFL-IN-ATTR                                       
009100                             PIC X(2).                                    
009200*                                 MFS ATTRIBUTFÄLT                        
009300        05 MOD-IDREFTAB-HFL-IN                                            
009400                             PIC X.                                       
009500*                                 HÖG FREKVENTA OCH LÅGA LEDTIDER         
009600        05 MOD-IDREFTAB-HFM-UT                                            
009700                             PIC X.                                       
009800*                                 HÖG FREKVENTA OCH MEDEL LEDTID          
009900        05 MOD-IDREFTAB-HFM-IN-ATTR                                       
010000                             PIC X(2).                                    
010100*                                 MFS ATTRIBUTFÄLT                        
010200        05 MOD-IDREFTAB-HFM-IN                                            
010300                             PIC X.                                       
010400*                                 HÖG FREKVENTA OCH MEDEL LEDTID          
010500        05 MOD-IDREFTAB-HFH-UT                                            
010600                             PIC X.                                       
010700*                                 HÖG FREKVENTA OCH HÖGA LEDTIDER         
010800        05 MOD-IDREFTAB-HFH-IN-ATTR                                       
010900                             PIC X(2).                                    
011000*                                 MFS ATTRIBUTFÄLT                        
011100        05 MOD-IDREFTAB-HFH-IN                                            
011200                             PIC X.                                       
011300*                                 HÖG FREKVENTA OCH HÖGA LEDTIDER         
011400        05 MOD-IDREFTAB-HFXH-UT                                           
011500                             PIC X.                                       
011600*                                 HÖG FREKVENT OCH X-HÖG LEDTIDER         
011700        05 MOD-IDREFTAB-HFXH-IN-ATTR                                      
011800                             PIC X(2).                                    
011900*                                 MFS ATTRIBUTFÄLT                        
012000        05 MOD-IDREFTAB-HFXH-IN                                           
012100                             PIC X.                                       
012200*                                 HÖG FREKVENT OCH X-HÖG LEDTIDER         
012300     03 MOD-TEMFSINF         PIC X(55).                                   
012400*                                 INFORMATIONSMEDDELANDE                  
012500*** END OF VILMAII-COPY LENGTH= 1149 BYTES                                
