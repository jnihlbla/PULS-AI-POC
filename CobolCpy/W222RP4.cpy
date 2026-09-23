000100 01  W222RP4.                                                             
000200*                                 UPPDATERING ANTALSMÄSSIGA               
000300*                                 PB-JUSTERINGAR                          
000400*                                                                         
000500     03 IDPTYP               PIC X(3).                                    
000600*                                 POSTTYP                                 
000700     03 IDARTNR              PIC S9(9)           COMP-3.                  
000800*                                 ARTIKELNUMMER                           
000900     03 KDCLAGER             PIC S9              COMP-3.                  
001000*                                 CENTRALLAGERKOD                         
001100     03 PB-JUSTERINGAR       OCCURS 2 TIMES.                              
001200        05 TIPBJUST          PIC S9(5)           COMP-3.                  
001300*                                 DATUM FÖR PB-JUSTERING (ÅÅVV)           
001400        05 KVPB-JUST         PIC S9(6)V9(1)      COMP-3.                  
001500*                                 PERIODBEHOVSJUSTERING                   
001600     03 FLABORT-PBJUST       PIC X.                                       
001700*                                 BORTTAG AV BEFINTLIG                    
001800*                                 PB-JUSTERING?                           
001900*** END COPY W222RP4CC0  LENGTH=24                                        
