//W221J159 JOB (670W2210100W221J159,W100),'RTN W224V1',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST2                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W221    EXEC W221P159,                                                        
//             INDIN2=W224.W224V1.W22416(+0)                                    
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W221J159                                         
