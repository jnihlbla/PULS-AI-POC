000100 01  W42851.                                                              
000200*                                 UPPFÖLJNINGS FIL                        
000300*                                 RETURER WEB-LDC/NDC                     
000400*                                 FOLLOW UP FILE FOR                      
000500*                                 RETURNS WEB-LDC/NDC       .             
000600     03 KDMFUP               PIC X(2).                                    
000700*                                 RAPPORTGRUPP  MA/CN/PF/NA               
000800*                                 REPORT GROUP  MA/CN/PF/NA               
000900     03 IDDC-RET             PIC X(2).                                    
001000*                                 MOTTAGANDE LAGER FÖR RETURER            
001100*                                 RECEIVING WAREHOUSE FOR RETURNS         
001200     03 IDARTNR              PIC 9(8).                                    
001300*                                 ARTIKELNUMMER                           
001400*                                 PART NUMBER                             
001500     03 KVLEVANM-BEKR        PIC 9(6).                                    
001600*                                 BEKRÄFTAT RETURANTAL                    
001700     03 SURADER              PIC 9(9).                                    
001800*                                 TOTALT ANTAL RADER                      
001900*                                 TOTAL NUMBER OF LINES                   
002000     03 SUARTSTD             PIC 9(8)V9(2).                               
002100*                                 SUMMA STANDARDPRIS RADVÄRDE             
002200*                                 SUM LINEVALUE STANDARD PRICE            
002300*** END OF VILMAII-COPY LENGTH= 37 BYTES                                  
