//W425J025 JOB (640W4250100W425J025,W100),'RTN W425D1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST4                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W425    EXEC W425P025                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W425J025                                         
