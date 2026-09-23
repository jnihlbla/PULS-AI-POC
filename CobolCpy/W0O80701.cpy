000100 01  W0O80701.                                                            
000200*                                 COPYTEXT FÖR MID                        
000300*                                 W0O80701                                
000400*                                                                         
000500     03 IDTRANS              PIC X(4).                                    
000600*                                 BILDNUMMER                              
000700     03 MESSAGE-RAD1         PIC X(40).                                   
000800*                                 MEDDELANDEFÄLT PÅ RAD 1                 
000900     03 KDEMBTYP-IN          PIC X(3).                                    
001000*                                 EMBALLAGETYP       KDEMBTYP-002         
001100     03 KDUPPD-BEEMBTYP-IN   PIC X.                                       
001200*                                 UPPDATERINGSTYP                         
001300     03 KDEMBTYP-UT          PIC X(3).                                    
001400*                                 EMBALLAGETYP       KDEMBTYP-002         
001500     03 KDUPPD-BEEMBTYP-UT   PIC X.                                       
001600*                                 UPPDATERINGSTYP                         
001700     03 FILLER               OCCURS 6 TIMES.                              
001800        05 BEEMBTYP-ATTR     PIC X(2).                                    
001900*                                 MFS ATTRIBUTFÄLT                        
002000        05 BEEMBTYP          PIC X(12).                                   
002100*                                 EMBALLAGETYPSTEXT  BEEMBTYP-002         
002200     03 MESSAGE-RAD23        PIC X(79).                                   
002300*                                 MEDDELANDEFÄLT PÅ RAD 23                
002400*** END OF VILMAII-COPY LENGTH= 215 BYTES                                 
