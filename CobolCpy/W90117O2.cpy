000100 01  RESP-W90117O2-CTX.                                                   
000200*                                 COPYTEXT TILL PROGRAM W90117O0          
000300*                                                                         
000400     03 RESP-IDDISTR         PIC 9(4).                                    
000500*                                 DISTRIKTNUMMER                          
000600     03 RESP-IDKUNDNR        PIC 9(6).                                    
000700*                                 KUNDNUMMER                              
000800     03 RESP-IDLEVART        PIC X(30).                                   
000900*                                 LEVERANTÖRENS ARTNR                     
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
002200     03 RESP-TIBERANK        PIC X(10).                                   
002300     03 RESP-TIAAAA-MM-DD REDEFINES RESP-TIBERANK.                        
002400        05 RESP-TIAAAA       PIC 9(4).                                    
002500*                                 ÅRTAL (ÅÅÅÅ)                            
002600        05 RESP-TEHYPHEN     PIC X.                                       
002700*                                 BINDESTRECK                             
002800        05 RESP-TIMM         PIC 9(2).                                    
002900*                                 MÅNAD (MM)                              
003000        05 RESP-TEHYPHEN     PIC X.                                       
003100*                                 BINDESTRECK                             
003200        05 RESP-TIDD         PIC 9(2).                                    
003300*                                 DAG I MÅNAD (DD)                        
003400     03 RESP-IDDC-LEV        PIC X(2).                                    
003500*                                 LEVERERANDE DC I EXPORTFLÖDET           
003600     03 RESP-PRINK           PIC 9(7)V9(2).                               
003700*                                 INKÖPSPRIS                              
003800     03 RESP-KDFEL           PIC X(3).                                    
003900*                                 FELKOD                                  
004000     03 RESP-IDLEVART-TILLK  PIC X(30).                                   
004100*                                 LEVERANTÖRENS ARTNR                     
004200*** END OF VILMAII-COPY LENGTH= 201 BYTES                                 
