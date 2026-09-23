//W231Y002 JOB (640W2310100W231Y002,W100),'RTN WYR001',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST2                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//W231    EXEC W231P002,INDUT=W231.WYR001                                       
//W23130.W23130D3 DD DUMMY                                                      
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W231Y002                                         
