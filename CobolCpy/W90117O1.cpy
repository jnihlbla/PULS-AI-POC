000100 01  RESP-W90117O1-CTX.                                                   
000200*                                 COPYTEXT TILL PROGRAM W90117O0          
000300*                                                                         
000400     03 RESP-IDDISTR         PIC 9(4).                                    
000500*                                 DISTRIKTNUMMER                          
000600     03 RESP-IDKUNDNR        PIC 9(6).                                    
000700*                                 KUNDNUMMER                              
000800     03 RESP-IDARTNR         PIC 9(8).                                    
000900*                                 ARTIKELNUMMER                           
001000     03 RESP-BEART           PIC X(25).                                   
001100*                                 ARTIKELBENÄMNING                        
001200     03 RESP-FLSVAR          PIC X.                                       
001300*                                 ALLMÄN SVARSFLAGGA                      
001400     03 RESP-KVLS-DLEV       PIC 9(7).                                    
001500*                                 LAGERSALDO HOS DIREKTLEVENATÖR          
001600     03 RESP-KDSORT          PIC X(2).                                    
001700*                                 SORT-KOD                                
001800     03 RESP-KDORDBEK        PIC X(2).                                    
001900*                                 ORDERBEKRÄFTELSEKOD                     
002000     03 RESP-TEORDBEK        PIC X(70).                                   
002100*                                 ORDERBEKRÄFTELSETEXT                    
002200     03 RESP-TIBERANK        PIC 9(6).                                    
002300*                                 BERÄKNAD ANKOMSTDATUM                   
002400     03 RESP-IDDC-LEV        PIC X(2).                                    
002500*                                 LEVERERANDE DC I EXPORTFLÖDET           
002600     03 RESP-PRINK           PIC 9(7)V9(2).                               
002700*                                 INKÖPSPRIS                              
002800     03 RESP-KDFEL           PIC X(3).                                    
002900*                                 FELKOD                                  
003000     03 RESP-IDARTNR-TILLK   PIC 9(8).                                    
003100*                                 TILLKOMMANDE ARTIKELNUMMER              
003200*** END OF VILMAII-COPY LENGTH= 153 BYTES                                 
