000100 01  W4I36301.                                                            
000200*                                 COPYTEXT FÖR MID W4I36301               
000300     03 IDPTIDTAB-IN         PIC X(2).                                    
000400*                                 PRODUKTIONSTIDTABELLSIDENTITET          
000500     03 IDPTIDTAB-UT         PIC X(2).                                    
000600*                                 PRODUKTIONSTIDTABELLSIDENTITET          
000700     03 IDDC-IN              PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900     03 IDDC-UT              PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100     03 COPY-TAB             PIC X(2).                                    
001200     03 JA-NEJ-SW            PIC X.                                       
001300*                                 ALLMÄN FLAGGA                           
001400     03 KVPTSORT-IN          PIC X(6).                                    
001500*                                 ANTAL I PTIDSTABELL                     
001600     03 INDATA               OCCURS 10 TIMES.                             
001700        05 KVPTSORT          PIC X(6).                                    
001800*                                 ANTAL I PTIDSTABELL                     
001900     03 KDSORT-IN            PIC X(2).                                    
002000*                                 SORT-KOD                                
002100*** END COPY W4I36301    LENGTH=79                                        
