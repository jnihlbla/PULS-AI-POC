000100 01  HDR-WFAKTHUV.                                                        
000200*                                 HEADER-POST TILL LASER-FAKTURA          
000300     03 FILLER               PIC X.                                       
000400     03 HDR-FAKTHEADER       PIC X(10).                                   
000500     03 FILLER               PIC X.                                       
000600     03 HDR-IDFTG            PIC 9(2).                                    
000700*                                 FÖRETAGSID EKONOM REDOVISNING           
000800     03 FILLER               PIC X.                                       
000900     03 HDR-IDPRT            PIC X(3).                                    
001000*                                 LOGISK PRINTERIDENTITET                 
001100     03 FILLER               PIC X(98).                                   
001200*** END COPY WFAKTHUV    LENGTH=116                                       
