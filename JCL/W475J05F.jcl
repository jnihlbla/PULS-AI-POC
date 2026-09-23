//W475J05F JOB (650W4750100W475J003,W100),'RTN W475M1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=M                                                          
/*JOBPARM LINES=999                                                             
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST4                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//W475    EXEC W475P05F                                                         
//*                                                                             
//FICHE1  EXEC COMGENER,DSIN='&&FICHE1',DISP=DELETE,                            
//             FORMS=A001,                                                      
//             KOMLIB='W.QASE.COMPARMS',                                        
//             KOMREQ='W.QASE.CONSTANT(W4755FC1)'                               
//*                                                                             
//FICHE2  EXEC COMGENER,DSIN='&&FICHE2',DISP=DELETE,                            
//             FORMS=A001,                                                      
//             KOMLIB='W.QASE.COMPARMS',                                        
//             KOMREQ='W.QASE.CONSTANT(W4755FC2)'                               
//*                                                                             
//FICHE3  EXEC COMGENER,DSIN='&&FICHE3',DISP=DELETE,                            
//             FORMS=A001,                                                      
//             KOMLIB='W.QASE.COMPARMS',                                        
//             KOMREQ='W.QASE.CONSTANT(W4755FC3)'                               
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W475J05F                                         
