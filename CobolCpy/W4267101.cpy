000100 01  W4267101-CTX.                                                        
000200*                                 KVALITETSINFO (W6D211)                  
000300*                                 F÷R E+ PGM                              
000400     03 IDARTNR              PIC 9(8).                                    
000500*                                 ARTIKELNUMMER                           
000600     03 KDPERSON             PIC Z(2)9.                                   
000700*                                 PERSONKOD                               
000800     03 KDKVAINF             PIC X.                                       
000900*                                 TYP AV KVAL.INFO F÷R ARTIKEL            
001000     03 TEKVAINF-INT         OCCURS 7 TIMES                               
001100                             PIC X(79).                                   
001200*                                 KVALITETS INFORMATION INTERNT           
001300     03 TIREGDAT             PIC 9(6).                                    
001400*                                 REGISTRERINGSDATUM (≈≈MMDD)             
001500*** END OF VILMAII-COPY LENGTH= 571 BYTES                                 
