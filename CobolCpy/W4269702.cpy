000100 01  W4269702.                                                            
000200*                                 KVALITET                                
000300*                                 COPY-TEXT FÖR KONVERTERING              
000400     03 IDARTNR              PIC S9(9)           COMP-3.                  
000500*                                 ARTIKELNUMMER                           
000600     03 IDLEVNR              PIC X(5).                                    
000700*                                 LEVERANTÖRNUMMER                        
000800     03 KDKVAKTL.                                                         
000900*                                 KVALITETSKONTROLL KOD                   
001000        05 KDKVATYP          PIC X.                                       
001100*                                 NORMAL/VERIFIKATIONS KVAL.KONTR         
001200        05 IDPROVPL-PRI      PIC X.                                       
001300*                                 PROVTAGNINGSPLAN PRIM.KONTROLL          
001400        05 IDPROVPL-SEK      PIC X.                                       
001500*                                 PROVTAGNINGSPLAN SEK.KONTROLL           
001600        05 KDKVAULG          PIC X.                                       
001700*                                 UNDERLAG FÖR KVALITETSKONTROLL          
001800     03 ADKVAULG             PIC X(2).                                    
001900*                                 PLATS UNDERLAG KVAL.KONTROLL            
002000     03 KVSKPLOT-PRI         PIC S9(3)           COMP-3.                  
002100*                                 SKIPLOT RÄKNARE PRIMÄR KONTROLL         
002200     03 KVSKPLOT-SEK         PIC S9(3)           COMP-3.                  
002300*                                 SKIPLOT RÄKNARE SEK. KONTROLL           
002400     03 KVSKPLOT-PRI-INIT    PIC S9(3)           COMP-3.                  
002500*                                 SKIPLOT RÄKNARE PRIMÄR KONTROLL         
002600     03 KVSKPLOT-SEK-INIT    PIC S9(3)           COMP-3.                  
002700*                                 SKIPLOT RÄKNARE SEK. KONTROLL           
002800     03 TIUPPDAT             PIC S9(7)           COMP-3.                  
002900*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
003000*** END OF VILMAII-COPY LENGTH= 28 BYTES                                  
