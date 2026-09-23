000010*** EDIT ALLOWED                                                          
000011                                                                          
000020*    EDI 001 BEGINNING OF TRANSMISSION                                    
000030*    ANVÄNDS FÖR IFCSUM MEDDELANDE                                        
000050*                                                                         
000060*    FUNCTION,                                                            
000070*    -BEGINNING OF TRANSMISSION                                           
000080*                                                                         
000100 01  WEDI0013.                                                            
000230     03 0013-IDPTYP                            PIC X(03).                 
000240*                                              001                        
000501     03 0013-LENGTH                            PIC 9(03).                 
000504*                                              LENGTH = 073               
000505*                                                                         
000522     03 0013-SNODE-SENDER-COMMON-NODE          PIC X(04).                 
000523*                                                                         
000524     03 0013-RNODE-RECIVER-COMMON-NODE         PIC X(04).                 
000525*                                                                         
000526     03 0013-VFILE-VIRTUAL-FILE-NAME           PIC X(08).                 
000527*                                                                         
000528     03 0013-VFDATE-VIRTUAL-FILE-DATE          PIC X(06).                 
000529*                                                                         
000530     03 0013-VFTIME-VIRTUAL-FILE-TIME          PIC X(06).                 
000531*                                                                         
000532     03 0013-SPACE                             PIC X(45).                 
000533*                                                                         
000550*** END OF VILMAII-COPY LENGTH=079                                        
