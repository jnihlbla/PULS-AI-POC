000100*** EDIT ALLOWED                                                          
000200*                            *************************************        
000300*                            *** ANVÄNDS FÖR ATT LANDKODER 3-POS          
000400*                            *** TILL TVÅSTÄLLIG LANDKOD IDLANDX2.        
000500*                            *** FÖR DIRECT-BUSINESS, ELLER               
000600*                            *** TVÄRTOM.                                 
000700*                            *************************************        
000800*                                                                         
000900 01  LAND-X2-VALUES.                                                      
001000************************************LANX2                                 
001100     03  FILLER    PIC X(5)  VALUE 'AUSAT'.                               
001200     03  FILLER    PIC X(5)  VALUE 'BELBE'.                               
001300     03  FILLER    PIC X(5)  VALUE 'BRIGB'.                               
001400     03  FILLER    PIC X(5)  VALUE 'CZECZ'.                               
124032     03  FILLER    PIC X(5)  VALUE 'DENDK'.                               
125033     03  FILLER    PIC X(5)  VALUE 'FINFI'.                               
126024     03  FILLER    PIC X(5)  VALUE 'FRAFR'.                               
126124     03  FILLER    PIC X(5)  VALUE 'GERDE'.                               
127024*    03  FILLER    PIC X(5)  VALUE 'GREXX'.                               
210024     03  FILLER    PIC X(5)  VALUE 'IREIE'.                               
220028     03  FILLER    PIC X(5)  VALUE 'ITAIT'.                               
221036     03  FILLER    PIC X(5)  VALUE 'JPAJP'.                               
230027     03  FILLER    PIC X(5)  VALUE 'NETNL'.                               
240031     03  FILLER    PIC X(5)  VALUE 'NORNO'.                               
240032     03  FILLER    PIC X(5)  VALUE 'POLPL'.                               
250030     03  FILLER    PIC X(5)  VALUE 'PORPT'.                               
260029     03  FILLER    PIC X(5)  VALUE 'SPAES'.                               
261034     03  FILLER    PIC X(5)  VALUE 'SWICH'.                               
270024*    03  FILLER    PIC X(5)  VALUE 'SWEXX'.                               
270025     03  FILLER    PIC X(5)  VALUE 'TURTR'.                               
300000*                                                                         
310024 01  LAND-X2-TAB          REDEFINES LAND-X2-VALUES.                       
320036     03  LAND-X2-ING      OCCURS 18 TIMES                                 
330024                          ASCENDING KEY IS LAND-SOK                       
340024                          INDEXED BY LAND-IX.                             
350024       05  LAND-SOK            PIC X(3).                                  
360024       05  LAND-IDLANDX2       PIC X(2).                                  
370025*                                                                         
380025 01  X2-LAND-VALUES.                                                      
390025************************************LANX2                                 
390135     03  FILLER    PIC X(5)  VALUE 'ATAUS'.                               
390235     03  FILLER    PIC X(5)  VALUE 'BEBEL'.                               
390335     03  FILLER    PIC X(5)  VALUE 'CHSWI'.                               
390336     03  FILLER    PIC X(5)  VALUE 'CZCZE'.                               
391029     03  FILLER    PIC X(5)  VALUE 'DEGER'.                               
391132     03  FILLER    PIC X(5)  VALUE 'DKDEN'.                               
392029     03  FILLER    PIC X(5)  VALUE 'ESSPA'.                               
393033     03  FILLER    PIC X(5)  VALUE 'FIFIN'.                               
393034     03  FILLER    PIC X(5)  VALUE 'FRFRA'.                               
411026     03  FILLER    PIC X(5)  VALUE 'GBBRI'.                               
416025*    03  FILLER    PIC X(5)  VALUE 'XXGRE'.                               
417025     03  FILLER    PIC X(5)  VALUE 'IEIRE'.                               
418028     03  FILLER    PIC X(5)  VALUE 'ITITA'.                               
418136     03  FILLER    PIC X(5)  VALUE 'JPJPA'.                               
419027     03  FILLER    PIC X(5)  VALUE 'NLNET'.                               
419131     03  FILLER    PIC X(5)  VALUE 'NONOR'.                               
419132     03  FILLER    PIC X(5)  VALUE 'PLPOL'.                               
419230     03  FILLER    PIC X(5)  VALUE 'PTPOR'.                               
419240     03  FILLER    PIC X(5)  VALUE 'TRTUR'.                               
419425*    03  FILLER    PIC X(5)  VALUE 'XXSWE'.                               
419625*                                                                         
419725 01  X2-LAND-TAB          REDEFINES X2-LAND-VALUES.                       
419836     03  X2-LAND-ING      OCCURS 18 TIMES                                 
419925                          ASCENDING KEY IS X2-SOK                         
420025                          INDEXED BY X2-IX.                               
420125       05  X2-SOK              PIC X(2).                                  
420225       05  X2-LAND             PIC X(3).                                  
421025*                                                                         
430001*                                                                         
440027*** END COPY W463LAND    LENGTH=90                                        
