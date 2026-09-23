000100 01  1206-WDGX1206.                                                       
000200*                                 NYUPPLAGDA RUBRIK-,TTEXT- OCH           
000300*                                 FOTNOTS-NUMMER (ÖVERSÄTTNING)           
000400*                                 NYCKEL: KY1206                          
000500*                                  (IDCATNR, KDLEX, IDLEXNR)              
000600     03 1206-IDCATNR         PIC 9(5).                                    
000700*                                 KATALOG-ID                              
000800     03 1206-KDLEX           PIC X.                                       
000900*                                 LEXIKONTYP                              
001000     03 1206-IDLEXNR         PIC 9(5).                                    
001100*                                 SÖKNUMMER LEXIKON.                      
001200*                                 KAN INNEHÅLLA RUBRIK-, TTEXT-           
001300*                                 ELLER FOTNOTS-NR.                       
001400     03 1206-IDUSER          PIC X(8).                                    
001500*                                 ANVÄNDARENS SÄKERHETS ID                
001600     03 1206-KDBEH           PIC X.                                       
001700*                                 BEHANDLINGSKOD                          
001800     03 1206-IDSKYLT-RAD     OCCURS 20 TIMES.                             
001900        05 1206-IDSKYLT      PIC X(3).                                    
002000*                                 NATIONALITETSTECKEN                     
002100*                                 SPRÅKIDENTIFIKATION                     
002200        05 1206-TIUPPDAT     PIC S9(7)           COMP-3.                  
002300*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
002400     03 1206-TENOTE          PIC X(40).                                   
002500*                                 NOTERINGSFÄLT                           
002600*** END OF VILMAII-COPY LENGTH= 200 BYTES                                 
