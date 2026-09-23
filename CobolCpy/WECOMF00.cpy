000100*** EDIT ALLOWED                                                          
010007 01  WECOMF00.                                                            
020007*                                 ECOM MESSAGE REQUEST PATH               
021007*                                          AND USER KEY                   
022000*                                                                         
023007     03 ECOM-REQ-PATH-PARM.                                               
023207        05 ECOM-REQ-DOCREF-LTH    PIC S9999 COMP-5 SYNC.                  
023309*                   INVOICE / CREDIT NUMBER                               
023409        05 ECOM-REQ-IDFINDOC      PIC X(9).                               
023607     03 ECOM-REQ-HEADER.                                                  
023807        05 ECOM-REQ-USER-KEY-LTH  PIC S9999 COMP-5 SYNC.                  
024007        05 ECOM-REQ-USER-KEY      PIC X(32).                              
025007*                                                                         
055000*** END OF VILMAII-COPY LENGTH=80                                         
