000100 01  MICA-W402MICA.                                                       
000200*                                 LAYOUT FÖR ORDER(ARTIKEL-INFO)          
000300*                                 SOM SKICKAS TILL FLS/MIC                
000400*                                 ANVÄNDS I W40293 SOM SKICKAR            
000500*                                 ORDER TILL FLS/MIC VIA MQ               
000600     03 MICA-IDPTYP          PIC X(3).                                    
000700*                                 POSTTYP                                 
000800     03 MICA-IDARTNR         PIC 9(8).                                    
000900*                                 ARTIKELNUMMER                           
001000     03 MICA-KDSORT          PIC X(3).                                    
001100     03 MICA-KVLEVART        PIC 9(7).                                    
001200*                                 LEVERERAT ANTAL STYCK                   
001300     03 MICA-PRARTSTD        PIC 9(7)V9(2).                               
001400*                                 ARTIKELSTANDARDPRIS                     
001500     03 MICA-SUARTSTD        PIC 9(8)V9(2).                               
001600*                                 SUMMA STANDARDPRIS RADVÄRDE             
001700     03 MICA-KDVALISO        PIC X(3).                                    
001800*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
001900     03 MICA-KDARTURS        PIC X(2).                                    
002000*                                 ARTIKELURSPRUNGSKOD                     
002100*** END OF VILMAII-COPY LENGTH= 45 BYTES                                  
