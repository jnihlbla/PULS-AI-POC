000010*** EDIT ALLOWED                                                          
000011                                                                          
000020*    EDI CNI CONSIGNMENT INFORMATION                                      
000030*    ANVÄNDS FÖR IFCSUM MEDDELANDE, GENERELL COPYTEXT                     
000040*                                                                         
000050*    FUNCTION,                                                            
000060*    -TO IDENTIFY A CONSIGNMENT FOR WHICH STATUS DETAILS ARE              
      *     GIVEN.                                                              
000070*                                                                         
000100 01  WEDICNI.                                                             
000230     03 CNI-IDPTYP                             PIC X(03).                 
000240*                                              CNI                        
000501     03 CNI-LENGTH                             PIC 9(03).                 
000502*                                              LENGTH = 084               
000503*                                                                         
000515     03 CNI-1490-CONSOLID-ITEM-NO              PIC 9(04).                 
000516*                                                                         
000517     03 CNI-C503-DOCUMENT-MESSAGE.                                        
000518*                                                                         
000519        05 CNI-1004-DOCUMENT-NUMBER            PIC X(35).                 
000518*                                                                         
000519        05 CNI-1373-DOCUMENT-STAT-COD          PIC X(03).                 
000560*                                                                         
000519        05 CNI-1366-DOCUMENT-SOURCE            PIC X(35).                 
000518*                                                                         
000519        05 CNI-3453-LANGUAGE-CODED             PIC X(03).                 
000503*                                                                         
000515     03 CNI-1312-CONSIG-LOAD-SEG-NO            PIC 9(04).                 
000518*                                                                         
000570*** END OF VILMAII-COPY LENGTH=90                                         
