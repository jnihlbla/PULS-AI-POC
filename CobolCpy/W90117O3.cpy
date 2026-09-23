000100 01  RESP-W90117O3-CTX.                                                   
000200*                                 COPYTEXT TILL PROGRAM W9011700          
000300*                                                                         
000400     03 RESP-IDDISTR         PIC 9(4).                                    
000500*                                 DISTRIKTNUMMER                          
000600     03 RESP-IDKUNDNR        PIC 9(6).                                    
000700*                                 KUNDNUMMER                              
000800     03 RESP-IDLEVART        PIC X(30).                                   
000900*                                 LEVERANTÖRENS ARTNR                     
001000     03 RESP-IDSPRAK         PIC X(2).                                    
001100*                                 2-STÄLLIG ISO SPRÅKKOD                  
001200     03 RESP-BEARTEXT        PIC X(100).                                  
001300*                                 UTÖKAD ARTIKELBENÄMNING                 
001400     03 RESP-KDSORT          PIC X(2).                                    
001500*                                 SORT-KOD                                
001600     03 RESP-PRINK           PIC 9(7)V9(2).                               
001700*                                 INKÖPSPRIS                              
001800     03 RESP-IDLEVART-TILLK  PIC X(30).                                   
001900*                                 LEVERANTÖRENS ARTNR                     
002000     03 RESP-KVLS-DLEV       PIC 9(7).                                    
002100*                                 LAGERSALDO HOS DIREKTLEVENATÖR          
002200     03 RESP-FLSVAR          PIC X.                                       
002300*                                 ALLMÄN SVARSFLAGGA                      
002400     03 RESP-IDDC-LEV        PIC X(2).                                    
002500*                                 LEVERERANDE DC I EXPORTFLÖDET           
002600     03 RESP-KDORDBEK        PIC X(2).                                    
002700*                                 ORDERBEKRÄFTELSEKOD                     
002800     03 RESP-TEORDBEK        PIC X(70).                                   
002900*                                 ORDERBEKRÄFTELSETEXT                    
003000     03 RESP-TIBERANK        PIC X(10).                                   
003100*                                 BERÄKNAD ANKOMSTDATUM                   
003200     03 RESP-TIDISPIN        PIC X(10).                                   
003300*                                 DISP-DATUM NÄSTA INLEV (ÅÅMMDD)         
003400     03 RESP-FLLDCKND        PIC X.                                       
003500*                                 FL LDC-KUND                             
003600*** END OF VILMAII-COPY LENGTH= 286 BYTES                                 
