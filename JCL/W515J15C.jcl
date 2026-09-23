//W515J15C JOB (650W5100100W515J15C,W100),'RTN W515D3',                         
//             USER=?,PASSWORD=?,CLASS=K                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST5                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W515    EXEC W515P15C                                                         
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W515J15C                                         
