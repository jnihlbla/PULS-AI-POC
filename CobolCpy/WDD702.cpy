000100 01  WDD702.                                                              
000200*                                 INFO OM TILLKOMMANDE ARTIKLAR           
000300*                                 FYSISK NYCKEL IDKORTNR                  
000400     03 FLTEXT               PIC X.                                       
000500*                                 FINNS TEXTINFORMATION ?                 
000600     03 IDKORTNR             PIC S9(3)           COMP-3.                  
000700*                                 KORTNUMMER                              
000800*                                 (RADLÖPNR FÖR ERSÄTTNINGSINFO)          
000900     03 TYP1.                                                             
001000        05 IDARTNR-TILLK     PIC S9(9)           COMP-3.                  
001100*                                 TILLKOMMANDE ARTIKELNUMMER              
001200        05 DIERS-TILLK       PIC S9(4)V9(3)      COMP-3.                  
001300*                                 TILLKOMMANDE ARTIKELANTAL               
001400        05 FILLER            PIC X(11).                                   
001500     03 TYP2 REDEFINES TYP1.                                              
001600        05 BEERS             PIC X(20).                                   
001700*                                 ERSÄTTNINGSTEXT                         
001800*** END COPY WDD702CCC0  LENGTH=23                                        
