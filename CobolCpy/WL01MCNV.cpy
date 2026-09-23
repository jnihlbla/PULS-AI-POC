000100 01  MCNV-AREA.                                                           
000200*                                 LINK AREA TO PGM WL01MCNV               
000300*                                 IT CONVERTS INFO AND ERROR              
000400*                                 MESSAGE NUMBERS TO CORRE-               
000500*                                 SPONDING TEXT. WL01MCNV SHOULD          
000600*                                 ONLY BE USED IN IN CONNECTION           
000700*                                 WITH THE PULS DC WEB SYSTEM.            
000800*                                                                         
000900*                                 INPUT ARGUMENTS:                        
001000*                                  IDSPRAK      DESIRED LANGUAGE          
001100*                                  IDMSG-INFO   INFO MSG CODE             
001200*                                  IDMSG-ERROR  ERROR MSG CODE            
001300*                                  IDELMT-ERROR ITEM NAME                 
001400*                                                                         
001500*                                 OUTPUT ARGUMENTS:                       
001600*                                  MFSFEL       OUTPUT MSG TEXT           
001700*                                  MFSINF       OUTPUT MSG TEXT           
001800*                                                                         
001900     03 MCNV-IDSPRAK         PIC X(2).                                    
002000*                                 2-STÄLLIG ISO SPRÅKKOD                  
002100*                                 2-LETTER ISO LANGUAGE CODE              
002200     03 MCNV-IDMSG-INFO      PIC X(3).                                    
002300*                                 INFORMATIONSMEDDELANDE ID               
002400*                                 INFORMATION MESSAGE ID                  
002500     03 MCNV-IDMSG-ERROR     PIC X(3).                                    
002600*                                 FELMEDDELANDE ID                        
002700*                                 ERROR MESSAGE ID                        
002800     03 MCNV-IDELMT-ERROR    PIC X(16).                                   
002900*                                 DATAELEMENTIDENTITET                    
003000*                                 DATA ITEM NAME                          
003100     03 MCNV-MFSFEL.                                                      
003200        05 MCNV-IDMFSFEL     PIC X(3).                                    
003300*                                 MFS FELMEDDELANDE NUMMER                
003400*                                 MFS ERROR MESSAGE NUMBER                
003500        05 MCNV-FILLER       PIC X.                                       
003600        05 MCNV-TEMFSFEL     PIC X(40).                                   
003700*                                 MFS FELMEDDELANDE                       
003800*                                 MFS ERROR MESSAGE                       
003900     03 MCNV-MFSINF.                                                      
004000        05 MCNV-IDMFSINF     PIC X(3).                                    
004100*                                 MFS INFO. MEDDELANDE NUMMER             
004200*                                 MFS INFO MESSAGE NUMBER                 
004300        05 MCNV-FILLER       PIC X.                                       
004400        05 MCNV-TEMFSINF     PIC X(55).                                   
004500*                                 INFORMATIONSMEDDELANDE                  
004600*                                 INFORMATION MESSAGE                     
004700*** END OF VILMAII-COPY LENGTH= 127 BYTES                                 
