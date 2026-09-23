//W475DLET JOB (650W4750100W475DLET,W100),'RTN W010D8',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//DLET   EXEC  PGM=IDCAMS                                                       
//SYSPRINT DD  SYSOUT=*                                                         
//SYSIN    DD  *                                                                
 DELETE (W475.BMP.W47511.*)     NONVSAM                                         
 DELETE (W475.W475D1.W47511.*)  NONVSAM                                         
 DELETE (W475.BMP.W47513.*)     NONVSAM                                         
 DELETE (W475.BMP.W47514.*)     NONVSAM                                         
 DELETE (W475.BMP.W47521.*)     NONVSAM                                         
 DELETE (W475.W475D1.W47521.*)  NONVSAM                                         
 DELETE (W475.BMP.W47541.*)     NONVSAM                                         
 DELETE (W475.W475D1.W47541.*)  NONVSAM                                         
 DELETE (W475.W475S1.W47553.*)  NONVSAM                                         
 DELETE (W475.W475S1.W4754A.*)  NONVSAM                                         
 DELETE (W475.BMP.W47531.*)     NONVSAM                                         
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W475DLET                                         
