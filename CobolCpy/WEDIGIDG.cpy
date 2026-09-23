000010*** EDIT ALLOWED                                                          
000011                                                                          
000020*    EDI GID GOODS ITEN DETAIS                                            
000030*    ANVÄNDS FÖR IFCSUM MEDDELANDE, GENERELL COPYTEXT                     
000031*                                                                         
000033*    FUNCTION,                                                            
000034*    -TO IDENTIFY A GOODS ITEM FOR WHICH TRANSPORT IS UNDERTAKEN.         
000036*     ONE GID-SEGMENT FOR EACH CMI GROUP.                                 
000037*                                                                         
000040*                                                                         
000100 01  WEDIGID.                                                             
000230     03 GID-IDPTYP                             PIC X(03).                 
000240*                                              GID                        
000501     03 GID-LENGTH                             PIC 9(03).                 
000502*                                              LENGTH = 203               
000503*                                                                         
000515     03 GID-1496-GOODS-ITEM-NUMBER             PIC 9(05).                 
000516*                                                                         
000518     03 GID-C213-NUMBER-OF-PACKAGES.                                      
000519*                                                                         
000520        05 GID-7224-NUMBER-OF-PACKAGES         PIC 9(08).                 
000521*                                                                         
000522        05 GID-7065-TYPE-OF-PACK-ID            PIC X(17).                 
000523*                                                                         
000520        05 GID-1131-CODE-LIST-QUAL             PIC X(03).                 
000521*                                                                         
000520        05 GID-3055-CODE-LIST-RESP-COD         PIC X(03).                 
000521*                                                                         
000524        05 GID-7064-TYPE-OF-PACKAGES           PIC X(35).                 
000569*                                                                         
000570*** END OF VILMAII-COPY LENGTH=209                                        
