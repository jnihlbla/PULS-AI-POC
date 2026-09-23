000100 01  4826-WDGX4826.                                                       
000200*                                 KVALITE                                 
000300*                                 FELKODER KONTROLLRAPPORTER              
000400*                                 FYSISK NYCKEL WDGXKEY:                  
000500*                                 (IDKRFEL  + LOW-VALUE)                  
000600     03 4826-IDKRFEL         PIC X(2).                                    
000700*                                 FELKOD FÖR KONTROLLRAPPORT              
000800*                                 ERRORCODE FOR INSP.REPORT               
000900     03 4826-LOW-VALUE       PIC X(8).                                    
001000     03 4826-BEKRFEL         PIC X(70).                                   
001100*                                 BESKRIVNING FELKOD KONTR.RAPPOR         
001200*                                 T                                       
001300*                                 DESCRIPTION OF ERRORCODE INSP.R         
001400*                                 EPORT                                   
001500     03 4826-FILLER          PIC X(20).                                   
001600*** END COPY WDGX4826C0  LENGTH=100                                       
