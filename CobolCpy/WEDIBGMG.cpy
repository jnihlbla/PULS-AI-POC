000010*** EDIT ALLOWED                                                          
000011                                                                          
000020*    EDI BGM BEGINNING OF MESSAGE                                         
000030*    ANVÄNDS FÖR IFCSUM MEDDELANDE, GENERELL COPYTEXT                     
000050*                                                                         
000060*    FUNCTION,                                                            
000070*    -TO TRANSMIT IDENTIFYING NUMBER AT MANIFEST LEVEL                    
000080*                                                                         
000100 01  WEDIBGM.                                                             
000230     03 BGM-IDPTYP                             PIC X(03).                 
000240*                                              BGM                        
000501     03 BGM-LENGTH                             PIC 9(03).                 
000504*                                              LENGTH = 085               
000505*                                                                         
000515     03 BGM-C002-DOCUMENT-NAME.                                           
000516*                                                                         
000518        05 BGM-1001-DOCUMENT-NAME              PIC X(03).                 
000519*                                                                         
000518        05 BGM-1131-CODE-LIST-QUAL             PIC X(03).                 
000519*                                                                         
000518        05 BGM-3055-CODE-LIST-RESP             PIC X(03).                 
000519*                                                                         
000520        05 BGM-1000-DOCUMENT-NAME              PIC X(35).                 
000521*                                                                         
000522     03 BGM-1004-DOCUMENT-NUMBER               PIC X(35).                 
000523*                                                                         
000526     03 BGM-1225-MESSAGE-FUNCTION              PIC X(03).                 
000523*                                                                         
000526     03 BGM-4343-RESPONSE-TYPE                 PIC X(03).                 
000542*                                                                         
000550*** END OF VILMAII-COPY LENGTH=91                                         
