000100 01  RESP-W50281O1.                                                       
000200*                                 RESPONSE-COPYTXT FÖR BILD 5281          
000300*                                 VID ANROP FRÅN WEBBEN                   
000400*                                 ACS-INVENTERING                         
000500     03 RESP-ACS-RE0-GRP.                                                 
000600*                                 RAPPORTERINGS-FÄLT                      
000700        05 RESP-PRAVCOST     PIC X(10).                                   
000800*                                 MEDELVÄRDESKOSTNAD I UTL.VALUTA         
000900        05 RESP-PREXAVCOST   PIC X(16).                                   
001000*                                 ALLMÄNT BELOPPSFÄLT (KR O ÖREN)         
001100*                                                                         
001200        05 RESP-FLONHAND     PIC X.                                       
001300*                                 ALLMÄN FLAGGA                           
001400        05 RESP-FLBUFFER     PIC X.                                       
001500*                                 ALLMÄN FLAGGA                           
001600        05 RESP-FLACSLOAD    PIC X.                                       
001700*                                 ALLMÄN FLAGGA                           
001800*** END OF VILMAII-COPY LENGTH= 29 BYTES                                  
