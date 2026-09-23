000100 01  RESP-W60139O1.                                                       
000200*                                 COPYTEXT FÖR RESP  W6013900             
000300     03 RESP-IDKR-START      PIC 9(5).                                    
000400*                                 KONTROLLRAPPORT NUMMER                  
000500     03 RESP-IDKR-NEXT       PIC 9(5).                                    
000600*                                 KONTROLLRAPPORT NUMMER                  
000700     03 RESP-IDKVAINF-START  PIC 9(2).                                    
000800*                                 RADNR FÖR KVALITETSKONTROLLTEXT         
000900     03 RESP-IDKVAINF-NEXT   PIC 9(2).                                    
001000*                                 RADNR FÖR KVALITETSKONTROLLTEXT         
001100     03 RESP-IDARTNR         PIC Z(8)9.                                   
001200*                                 ARTIKELNUMMER                           
001300     03 RESP-BEART           PIC X(25).                                   
001400*                                 ARTIKELBENÄMNING                        
001500     03 RESP-BELEVART        PIC X(30).                                   
001600*                                 LEVERANTÖRENS ARTIKELBENÄMNING          
001700     03 RESP-IDLEVNR         PIC X(5).                                    
001800*                                 LEVERANTÖRNUMMER                        
001900     03 RESP-KVAVIS          PIC Z(6)9.                                   
002000*                                 AVISERAT ANTAL                          
002100     03 RESP-FLAGGA-KR-HOPP-ATTR                                          
002200                             PIC X(2).                                    
002300*                                 MFS ATTRIBUTFÄLT                        
002400     03 RESP-FLAGGA-KR-HOPP  PIC X.                                       
002500*                                 GOKDKÄND                                
002600     03 RESP-KVKVAPRIM       PIC Z(5)9.                                   
002700*                                 ANTAL TILL PRIMÄRKONTROLL               
002800     03 RESP-IDUSER-PRI-ATTR PIC X(2).                                    
002900*                                 MFS ATTRIBUTFÄLT                        
003000     03 RESP-IDUSER-PRI      PIC X(8).                                    
003100*                                 ANVÄNDARENS SÄKERHETS ID                
003200     03 RESP-FLAGGA-PRI-ATTR PIC X(2).                                    
003300*                                 MFS ATTRIBUTFÄLT                        
003400     03 RESP-FLAGGA-PRI      PIC X.                                       
003500*                                 GOKDKÄND                                
003600     03 RESP-BEANST-ATTR     PIC X(2).                                    
003700*                                 MFS ATTRIBUTFÄLT                        
003800     03 RESP-BEANST          PIC X(25).                                   
003900*                                 ANSTÄLLDS NAMN                          
004000     03 RESP-KVKVASEK        PIC Z(5)9.                                   
004100*                                  ANTAL TILL SEKUNDÄRKONTROLL            
004200     03 RESP-IDUSER-SEK-ATTR PIC X(2).                                    
004300*                                 MFS ATTRIBUTFÄLT                        
004400     03 RESP-IDUSER-SEK      PIC X(8).                                    
004500*                                 ANVÄNDARENS SÄKERHETS ID                
004600     03 RESP-FLAGGA-SEK-ATTR PIC X(2).                                    
004700*                                 MFS ATTRIBUTFÄLT                        
004800     03 RESP-FLAGGA-SEK      PIC X.                                       
004900*                                 GOKDKÄND                                
005000     03 RESP-IDUSER-ADM-ATTR PIC X(2).                                    
005100*                                 MFS ATTRIBUTFÄLT                        
005200     03 RESP-IDUSER-ADM      PIC X(8).                                    
005300*                                 ANVÄNDARENS SÄKERHETS ID                
005400     03 RESP-FLAGGA-ADM-ATTR PIC X(2).                                    
005500*                                 MFS ATTRIBUTFÄLT                        
005600     03 RESP-FLAGGA-ADM      PIC X.                                       
005700*                                 GOKDKÄND                                
005800     03 RESP-KDKVATYP        PIC X(15).                                   
005900     03 RESP-ADKVAULG        PIC X(2).                                    
006000*                                 PLATS UNDERLAG KVAL.KONTROLL            
006100     03 RESP-UNDERLAG        PIC X(15).                                   
006200     03 RESP-SPEC-BEANST     PIC X(25).                                   
006300*                                 ANSTÄLLDS NAMN                          
006400     03 RESP-KONTROLL        PIC X(17).                                   
006500     03 RESP-IDKR            PIC 9(5).                                    
006600*                                 KONTROLLRAPPORT NUMMER                  
006700     03 RESP-FLAGGA-TEXT     PIC X(9).                                    
006800     03 RESP-FLAGGA-GODK-ATTR                                             
006900                             PIC X(2).                                    
007000*                                 MFS ATTRIBUTFÄLT                        
007100     03 RESP-FLAGGA-GODK     PIC X.                                       
007200*                                 GOKDKÄND                                
007300     03 RESP-EMPLID-TEXT     PIC X(8).                                    
007400     03 RESP-IDUSER-APR-ATTR PIC X(2).                                    
007500*                                 MFS ATTRIBUTFÄLT                        
007600     03 RESP-IDUSER-APR      PIC X(8).                                    
007700*                                 ANVÄNDARENS SÄKERHETS ID                
007800     03 RESP-IDTFN           PIC X(20).                                   
007900*                                 TELEFONNUMMER EXTERNT                   
008000     03 RESP-TEKRFEL         OCCURS 4 TIMES                               
008100                             PIC X(70).                                   
008200*                                 FELBESKRIVNING I FRI TEXT               
008300*** END OF VILMAII-COPY LENGTH= 581 BYTES                                 
