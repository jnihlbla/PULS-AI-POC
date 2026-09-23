000100 01  W4O36301.                                                            
000200*                                 COPYTEXT FÖR MOD                        
000300*                                 W4O36301                                
000400     03 IDTRANS              PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 TEMFSFEL             PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 IDPTIDTAB-IN         PIC X(2).                                    
000900*                                 PRODUKTIONSTIDTABELLSIDENTITET          
001000     03 IDPTIDTAB-UT         PIC X(2).                                    
001100*                                 PRODUKTIONSTIDTABELLSIDENTITET          
001200     03 IDDC-IN              PIC X(2).                                    
001300*                                 IDENTIFIERARE LAGER                     
001400     03 IDDC-UT              PIC X(2).                                    
001500*                                 IDENTIFIERARE LAGER                     
001600     03 COPY-TAB-ATTR        PIC X(2).                                    
001700*                                 MFS ATTRIBUTFÄLT                        
001800     03 COPY-TAB             PIC X(2).                                    
001900     03 JA-NEJ-SW-ATTR       PIC X(2).                                    
002000*                                 MFS ATTRIBUTFÄLT                        
002100     03 JA-NEJ-SW            PIC X.                                       
002200*                                 ALLMÄN FLAGGA                           
002300     03 KDSORT-Y-ATTR        PIC X(2).                                    
002400*                                 MFS ATTRIBUTFÄLT                        
002500     03 KDSORT-Y             PIC X(2).                                    
002600*                                 SORT-KOD                                
002700     03 RAD                  OCCURS 10 TIMES.                             
002800        05 KVPTSORT-Y-ATTR   PIC X(2).                                    
002900*                                 MFS ATTRIBUTFÄLT                        
003000        05 KVPTSORT-Y        PIC Z(3)9.9.                                 
003100*                                 ANTAL I PTIDSTABELL                     
003200        05 KOL               OCCURS 10 TIMES.                             
003300           07 KVPTID-ATTR    PIC X(2).                                    
003400*                                 MFS ATTRIBUTFÄLT                        
003500           07 KVPTID         PIC Z9.9.                                    
003600*                                 GENOMSNITTLIG TID/RAD MINUTER           
003700     03 X-AXEL               OCCURS 10 TIMES.                             
003800        05 KVPTSORT-X-ATTR   PIC X(2).                                    
003900*                                 MFS ATTRIBUTFÄLT                        
004000        05 KVPTSORT-X        PIC Z(3)9.9.                                 
004100*                                 ANTAL I PTIDSTABELL                     
004200     03 KDSORT-X-ATTR        PIC X(2).                                    
004300*                                 MFS ATTRIBUTFÄLT                        
004400     03 KDSORT-X             PIC X(2).                                    
004500*                                 SORT-KOD                                
004600     03 KVPTSORT-IN-ATTR     PIC X(2).                                    
004700*                                 MFS ATTRIBUTFÄLT                        
004800     03 KVPTSORT-IN          PIC X(2).                                    
004900*                                 MFS BEHANDLING AV INPUTFÄLT             
005000     03 INDATA               OCCURS 10 TIMES.                             
005100        05 KVPTSORT-ATTR     PIC X(2).                                    
005200*                                 MFS ATTRIBUTFÄLT                        
005300        05 KVPTSORT          PIC X(2).                                    
005400*                                 MFS BEHANDLING AV INPUTFÄLT             
005500     03 KDSORT-IN-ATTR       PIC X(2).                                    
005600*                                 MFS ATTRIBUTFÄLT                        
005700     03 KDSORT-IN            PIC X(2).                                    
005800*                                 SORT-KOD                                
005900     03 TEMFSINF             PIC X(55).                                   
006000*                                 INFORMATIONSMEDDELANDE                  
006100*** END COPY W4O36301    LENGTH=930                                       
