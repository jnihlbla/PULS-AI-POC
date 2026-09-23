000100 01  DOC-WL01061.                                                         
000200*                                 COPYTEXT FOR LOACTION QUERY  LD         
000300*                                 C                                       
000400     03 DOC-IDAFPRCD         PIC X(10).                                   
000500*                                 AFP-BLANKETT POSTTYP                    
000600     03 DOC-IDDC             PIC X(2).                                    
000700*                                 IDENTIFIERARE LAGER                     
000800     03 DOC-IDARTNR          PIC Z(7)9.                                   
000900*                                 ARTIKELNUMMER                           
001000     03 DOC-BEART            PIC X(25).                                   
001100*                                 ARTIKELBENÄMNING                        
001200     03 DOC-ADLAGOMR         PIC 9(2).                                    
001300*                                 LAGEROMRÅDE                             
001400     03 DOC-ADGANG           PIC 9(2).                                    
001500*                                 GÅNG                                    
001600     03 DOC-ADPLATS          PIC 9(5).                                    
001700*                                 LAGERPLATSNUMMER                        
001800     03 DOC-KVLS             PIC -(7)9.                                   
001900*                                 LAGERSALDO                              
002000     03 DOC-KVPB-REF         PIC Z(5)9.9.                                 
002100*                                 PERIODBEHOV REFILLING                   
002200*** END OF VILMAII-COPY LENGTH= 70 BYTES                                  
