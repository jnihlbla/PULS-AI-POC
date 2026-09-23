000100 01  RESP-W90118O1-CTX.                                                   
000200*                                 COPYTEXT TILL PROGRAM W90118O0          
000300*                                                                         
000400     03 RESP-IDARTNR         PIC 9(8).                                    
000500*                                 ARTIKELNUMMER                           
000600     03 RESP-IDDC            PIC X(2).                                    
000700*                                 IDENTIFIERARE LAGER                     
000800     03 RESP-BEART           PIC X(25).                                   
000900*                                 ARTIKELBENÄMNING                        
001000     03 RESP-KVLS            PIC 9(7).                                    
001100*                                 LAGERSALDO                              
001200     03 RESP-KDSORT          PIC X(2).                                    
001300*                                 SORT-KOD                                
001400     03 RESP-KDORDBEK        PIC X(2).                                    
001500*                                 ORDERBEKRÄFTELSEKOD                     
001600     03 RESP-TEORDBEK        PIC X(70).                                   
001700*                                 ORDERBEKRÄFTELSETEXT                    
001800     03 RESP-TIBERANK        PIC 9(6).                                    
001900*                                 BERÄKNAD ANKOMSTDATUM                   
002000     03 RESP-IDARTNR-TILLK   PIC 9(8).                                    
002100*                                 TILLKOMMANDE ARTIKELNUMMER              
002200     03 RESP-FLSVAR          PIC X.                                       
002300*                                 ALLMÄN SVARSFLAGGA                      
002400     03 RESP-KDFEL           PIC X(3).                                    
002500*                                 FELKOD                                  
002600*** END OF VILMAII-COPY LENGTH= 134 BYTES                                 
