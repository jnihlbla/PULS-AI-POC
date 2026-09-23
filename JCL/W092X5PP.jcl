//W092X5PP JOB (640W0920100W092X5PP,W100),'RTN W092X5',                         
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
/*JOBPARM FORMS=1800,LINECT=0                                                   
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*ROUTE  XEQ   LOCAL                                                            
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//*                                                                             
//*  DENNA JCL ÄR STARTAD AV VCOM                                               
//*                                                                             
//SOP     EXEC WSOP,                                                            
//             SOPREG=W.QASE.SOP,                                               
//             PDSLIB=W.QASE.JCL,PDSTEMP=W.QASETMP.JCL                          
  ORDER W092X5 SYMBOLS                                                          
    VCOM(W092X5PP)                                                              
  END-ORDER                                                                     
//*                                                                             
